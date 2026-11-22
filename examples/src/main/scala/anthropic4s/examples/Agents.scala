/*
 * Copyright 2026 anthropic4s contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package anthropic4s.examples

import anthropic4s._
import anthropic4s.otel.{ContentMode, OtelConfig, OtelTelemetry}
import anthropic4s.playjson.PlaySchema
import anthropic4s.schema.{description, Schema}
import cats.effect.{IO, IOApp}
import cats.syntax.all._
import io.circe.syntax._
import org.typelevel.otel4s.oteljava.OtelJava
import play.api.libs.json.{Json, JsonConfiguration, OFormat}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

final case class GetWeather(@description("City name, e.g. Brussels") city: String)
final case class Weather(city: String, celsius: Double, conditions: String)
final case class BookTable(
    restaurant: String,
    @description("ISO date, e.g. 2026-12-24") date: String,
    guests: Int
)

object Tools {

  // A tool is a function from a case class to an effect. The argument schema is derived from the case
  // class, sent to the API as a strict tool, and the arguments are decoded before the function runs.
  val weather: Tool[IO] =
    Tool("get_weather", "Current weather for a city")((in: GetWeather) =>
      IO.pure(Weather(in.city, 7.5, "light rain"))
    )

  val bookTable: Tool[IO] =
    Tool("book_table", "Books a restaurant table. Costs money; only call when the user asked for a booking.") {
      (in: BookTable) =>
        IO.pure(s"Booked ${in.restaurant} on ${in.date} for ${in.guests}.")
    }
}

/** An agent loop: the model calls tools until it can answer. Prints what happens as it happens. */
object WeatherAgent extends IOApp.Simple {

  def run: IO[Unit] = Anthropic4s.fromEnv[IO].use(agent)

  /** The agent itself, for any client; `Traced` runs the same code with tracing switched on. */
  def agent(client: AnthropicClient[IO]): IO[Unit] = {
    val request = MessageRequest(
      Model.ClaudeOpus5_5,
      maxTokens = 64000,
      MessageParam.user(
        "Should I eat outside in Brussels tonight? If not, book Chez Leon for two on 2026-12-24."
      )
    ).withThinking(ThinkingConfig.adaptive)
      // If a safety classifier declines the request, let the API retry on a fallback model.
      .withFallbacks(Fallbacks.Default)

    val config = RunnerConfig
      .default[IO]
      .withMaxIterations(10)
      .withRequestOptions(RequestOptions.default.withBetas(Beta.ServerSideFallback))
      // An approval gate: anything that spends money is checked before it runs.
      .withBeforeTool { call =>
        if (call.name == "book_table")
          IO.println(s"\n[approval] booking requested: ${call.input.asJson.noSpaces}")
            .as(ToolDecision.Allow)
        else IO.pure(ToolDecision.Allow)
      }

    client.messages
      .toolRunner(List(Tools.weather, Tools.bookTable), config)
      .events(request)
      .evalMap {
        case RunEvent.Model(_, StreamEvent.ContentBlockDelta(_, Delta.Text(text))) => IO.print(text)
        case RunEvent.ToolStarted(call)                                            =>
          IO.println(s"\n[tool] ${call.name} ${call.input.asJson.noSpaces}")
        case RunEvent.ToolFinished(_, result) => IO.println(s"[tool] -> ${result.text}")
        case RunEvent.Finished(result)        =>
          // How the run ended is a value: a refusal or a token limit is handled here, not caught.
          result.stop match {
            case RunStop.Completed =>
              IO.println(s"\n\ndone in ${result.iterations} turns, ${result.usage.show}")
            case RunStop.Refused(_) => IO.println("\nThe model declined.")
            case other              => IO.println(s"\nStopped early: $other")
          }
        case _ => IO.unit
      }
      .compile
      .drain
  }
}

final case class Ticket(
    @description("One-line summary") title: String,
    urgent: Boolean,
    assignee: Option[String]
)
object Ticket {
  // The settings that make Json.format agree with the derived schema (nulls written, `type` tags).
  implicit val config: JsonConfiguration = PlaySchema.configuration
  implicit val format: OFormat[Ticket]   = Json.format[Ticket]
  implicit val schema: Schema[Ticket]    = PlaySchema.derived[Ticket]
}

/** Structured output read with a Play JSON `Format`. */
object PlayJsonOutput extends IOApp.Simple {

  def run: IO[Unit] =
    Anthropic4s.fromEnv[IO].use { client =>
      val request = MessageRequest(
        Model.ClaudeOpus5_5,
        16000,
        MessageParam.user("File a ticket: the office printer is on fire.")
      )
      client.messages
        .parse[Ticket](request)
        .flatMap(parsed => IO.println(Json.prettyPrint(Json.toJson(parsed.value))))
    }
}

/**
 * The weather agent again, traced with OpenTelemetry. Spans go over OTLP to `localhost:4318` unless
 * `OTEL_EXPORTER_OTLP_ENDPOINT` says otherwise; `docker compose up -d` in this repository starts a collector
 * with Grafana on http://localhost:3000.
 */
object Traced extends IOApp.Simple {

  def run: IO[Unit] =
    OtelJava.autoConfigured[IO]().use { otel =>
      for {
        // Content mode also records prompts, replies and tool arguments, which makes a demo trace
        // readable. The default, MetadataOnly, is the right choice for real user data.
        telemetry <- OtelTelemetry[IO](
          otel.tracerProvider,
          OtelConfig.default.withContentMode(ContentMode.Content)
        )
        config <- AnthropicConfig.fromEnv[IO]
        // One trace: the runner span, a span per request, a span per tool call.
        _ <- Anthropic4s.resource[IO](config, telemetry).use(WeatherAgent.agent)
      } yield ()
    }
}

/** The same SDK from `Future` code, as in a Play application. No `IO` in this program's logic. */
object FutureStyle {

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val anthropic = FutureAnthropic.fromEnv()
    val request   = MessageRequest(Model.ClaudeOpus5_5, 16000, MessageParam.user("Name three Belgian beers."))

    val answer: Future[String] =
      anthropic
        .create(request)
        .map(_.text)
        .recover { case e: AnthropicError.Api => s"The API said ${e.status}: ${e.errorMessage}" }

    println(Await.result(answer, 2.minutes))
    Await.result(anthropic.close(), 10.seconds)
  }
}
