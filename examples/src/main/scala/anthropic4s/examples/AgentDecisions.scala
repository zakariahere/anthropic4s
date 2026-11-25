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
import anthropic4s.decide._
import cats.effect.{IO, IOApp}
import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import org.http4s.ember.client.EmberClientBuilder

import scala.concurrent.duration.Duration

sealed trait Intent
object Intent {
  case object Chat    extends Intent
  case object Weather extends Intent
  case object Booking extends Intent
}

/**
 * An agent with a small decision model in front of it and beside it. Kev-4B makes the quick calls: it routes
 * each request to the cheapest setup that can handle it, and it checks every booking before it runs. Claude
 * does the thinking. Needs llama-server serving Kev-4B on port 8081, and `ANTHROPIC_API_KEY`.
 */
object AgentDecisions extends IOApp.Simple {

  // Describe what each option needs, not what it is about: "eat outside tonight?" never says "weather".
  // These descriptions got the top choice right on every test request in a live run. A request with two
  // intents (weather, then a booking as a fallback) still comes back unsure, and goes to the full agent.
  val intent = Question.choice[Intent]("What does the user need?")(
    Criterion(
      Intent.Chat,
      "chat",
      "answerable from general knowledge, without live data and without taking any action"
    ),
    Criterion(
      Intent.Weather,
      "weather",
      "needs today's weather to answer, for example whether to go outside"
    ),
    Criterion(
      Intent.Booking,
      "booking",
      "contains a request to book or reserve something, even conditionally"
    )
  )

  // Describing true and false matters here: it moved a real booking request from 0.57 to 0.69, and a
  // weather-only question from 0.08 to 0.11, in a live run.
  val bookingAsked = Question
    .noul("Did the user ask for a restaurant booking?")
    .describe(
      whenTrue = "the user asks to book, reserve or get a table somewhere, even conditionally",
      whenFalse = "the user only asks for information or advice"
    )

  val allTools: List[Tool[IO]] = List(Tools.weather, Tools.bookTable)

  val requests = List(
    "What is the capital of Belgium?",
    "Should I eat outside in Brussels tonight?",
    "Should I eat outside in Brussels tonight? If not, book Chez Leon for two on 2026-12-24."
  )

  def run: IO[Unit] =
    // One HTTP client for both models.
    EmberClientBuilder
      .default[IO]
      .withTimeout(Duration.Inf) // each client applies its own timeouts
      .withIdleConnectionTime(Duration.Inf)
      .build
      .use { http =>
        AnthropicConfig.fromEnv[IO].flatMap { config =>
          val claude = AnthropicClient.fromClient(http, config)
          val decide = Decide.fromClient(http, DecideConfig.local())
          requests.traverse_ { text =>
            IO.println(s"\n> $text") *> handle(claude, decide, text).flatMap(IO.println)
          }
        }
      }

  /** Routes first: a confident answer picks a small model and only the tools it needs. */
  def handle(claude: AnthropicClient[IO], decide: Decide[IO], text: String): IO[String] =
    decide.ask(text, intent).flatMap { route =>
      val label = f"[route] ${route.choice} (confidence ${route.confidence}%.2f)"
      val reply =
        if (route.confidence < 0.8) agent(claude, decide, Model.ClaudeOpus5_5, allTools, text)
        else
          route.choice match {
            case Intent.Chat    => chat(claude, text)
            case Intent.Weather => agent(claude, decide, Model.ClaudeHaiku5_5, List(Tools.weather), text)
            case Intent.Booking => agent(claude, decide, Model.ClaudeSonnet5_5, allTools, text)
          }
      IO.println(label) *> reply
    }

  /** No tools needed: one plain request to the smallest model. */
  def chat(claude: AnthropicClient[IO], text: String): IO[String] =
    claude.messages
      .create(MessageRequest(Model.ClaudeHaiku5_5, 1024, MessageParam.user(text)))
      .map(reply => if (reply.isRefusal) "The model declined to answer." else reply.text)

  def agent(
      claude: AnthropicClient[IO],
      decide: Decide[IO],
      model: Model,
      tools: List[Tool[IO]],
      text: String
  ): IO[String] = {
    val config = RunnerConfig
      .default[IO]
      .withMaxIterations(10)
      .withBeforeTool(bookingGuard(decide, text))

    claude.messages.toolRunner(tools, config).run(MessageRequest(model, 16000, MessageParam.user(text))).map {
      result =>
        result.stop match {
          case RunStop.Completed  => result.text
          case RunStop.Refused(_) => "The model declined."
          case other              => s"Stopped early: $other"
        }
    }
  }

  /**
   * Checks every booking against what the user actually wrote, before it runs. A denial goes back to Claude
   * with its reason, so the agent changes course instead of failing. It fails closed: if the decision model
   * cannot answer, nothing is booked.
   */
  def bookingGuard(decide: Decide[IO], userText: String): ContentBlock.ToolUse => IO[ToolDecision] = call =>
    if (call.name != "book_table") IO.pure(ToolDecision.Allow)
    else
      decide
        .ask(conversation(userText), bookingAsked)
        .flatTap(p => IO.println(f"[guard] booking asked: $p%.2f"))
        .map { p =>
          if (p >= 0.5) ToolDecision.Allow
          else ToolDecision.Deny("The user did not ask for a booking. Do not book anything.")
        }
        .handleError(e => ToolDecision.Deny(s"Bookings cannot be checked right now (${e.getMessage})."))

  /** The state a System One server reads as a conversation. */
  def conversation(userText: String): Json =
    Json.arr(Json.obj("role" -> "user".asJson, "content" -> userText.asJson))
}
