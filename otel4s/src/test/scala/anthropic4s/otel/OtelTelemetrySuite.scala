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

package anthropic4s.otel

import anthropic4s._
import anthropic4s.syntax._
import anthropic4s.testkit.{Replies, StreamEvents}
import cats.effect.{IO, Ref}
import io.circe.Json
import io.circe.syntax._
import io.opentelemetry.api.common.AttributeKey
import io.opentelemetry.api.trace.propagation.W3CTraceContextPropagator
import io.opentelemetry.api.trace.{SpanKind, StatusCode}
import io.opentelemetry.sdk.trace.data.SpanData
import munit.CatsEffectSuite
import org.http4s.client.Client
import org.http4s.{Header, HttpApp, Request, Response, Status}
import org.typelevel.ci.CIString
import org.typelevel.otel4s.oteljava.testkit.trace.TracesTestkit
import org.typelevel.otel4s.trace.Tracer

final case class Lookup(city: String)

class OtelTelemetrySuite extends CatsEffectSuite {

  private val request = MessageRequest(Model.ClaudeOpus5_5, 1024, MessageParam.user("Secret question"))
    .withSystem("Secret instructions")
    .withEffort(Effort.High)

  private def json(message: Message): Response[IO] =
    Response[IO](Status.Ok)
      .withEntity(message.asJson.noSpaces)
      .putHeaders(Header.Raw(CIString("request-id"), "req_9"))

  private def sse(message: Message): Response[IO] =
    Response[IO](Status.Ok).withEntity(
      StreamEvents
        .of(message)
        .map { e =>
          val body = e.asJson
          s"event: ${body.hcursor.get[String]("type").getOrElse("")}\ndata: ${body.noSpaces}\n\n"
        }
        .mkString
    )

  private def error(status: Int, tpe: String): Response[IO] =
    Response[IO](Status.fromInt(status).fold(throw _, identity))
      .withEntity(s"""{"type":"error","error":{"type":"$tpe","message":"nope"}}""")

  final private class Env(
      val sdk: AnthropicClient[IO],
      val tracer: Tracer[IO],
      kit: TracesTestkit[IO],
      val seen: Ref[IO, Vector[Request[IO]]]
  ) {
    def spans: IO[List[SpanData]] = kit.finishedSpans
  }

  /** A client wired to an in-memory span exporter, answering with the given responses in order. */
  private def env(config: OtelConfig, responses: Response[IO]*)(test: Env => IO[Unit]): IO[Unit] =
    TracesTestkit.inMemory[IO](_.addTextMapPropagators(W3CTraceContextPropagator.getInstance())).use { kit =>
      for {
        seen <- Ref.of[IO, Vector[Request[IO]]](Vector.empty)
        http = Client.fromHttpApp(
          HttpApp[IO](req => seen.modify(rs => (rs :+ req, responses(math.min(rs.size, responses.size - 1)))))
        )
        telemetry <- OtelTelemetry[IO](kit.tracerProvider, config)
        tracer    <- kit.tracerProvider.get("test")
        sdk = AnthropicClient.fromClient[IO](
          http,
          AnthropicConfig("sk-test")
            .withRetryPolicy(RetryPolicy.default.withInitialDelay(scala.concurrent.duration.Duration.Zero)),
          telemetry
        )
        _ <- test(new Env(sdk, tracer, kit, seen))
      } yield ()
    }

  private def str(span: SpanData, key: String): Option[String] = Option(
    span.getAttributes.get(AttributeKey.stringKey(key))
  )
  private def long(span: SpanData, key: String): Option[Long] =
    Option(span.getAttributes.get(AttributeKey.longKey(key))).map(_.longValue)
  private def has(span: SpanData, key: String): Boolean =
    span.getAttributes.asMap.keySet.stream.anyMatch(_.getKey == key)

  test("a request produces one CLIENT span with model, usage and ids, and no content by default") {
    env(OtelConfig.default, json(Replies.text("Secret answer"))) { e =>
      for {
        _     <- e.sdk.messages.create(request)
        spans <- e.spans
      } yield {
        assertEquals(spans.map(_.getName), List("anthropic.messages.create"))
        val span = spans.head
        assertEquals(span.getKind, SpanKind.CLIENT)
        assertEquals(span.getInstrumentationScopeInfo.getName, "anthropic4s")
        assertEquals(str(span, "gen_ai.provider.name"), Some("anthropic"))
        assertEquals(str(span, "gen_ai.operation.name"), Some("chat"))
        assertEquals(str(span, "gen_ai.request.model"), Some("claude-opus-5-5"))
        assertEquals(long(span, "gen_ai.request.max_tokens"), Some(1024L))
        assertEquals(str(span, "gen_ai.request.reasoning.level"), Some("high"))
        assertEquals(str(span, "gen_ai.response.id"), Some("msg_test"))
        assertEquals(long(span, "gen_ai.usage.input_tokens"), Some(10L))
        assertEquals(long(span, "gen_ai.usage.output_tokens"), Some(5L))
        assertEquals(str(span, "anthropic.message.stop_reason"), Some("end_turn"))
        assertEquals(str(span, "anthropic.request.id"), Some("req_9"))
        assertEquals(long(span, "http.response.status_code"), Some(200L))
        // Nothing the user or the model wrote leaves the process in the default mode.
        assert(!span.getAttributes.toString.contains("Secret"), span.getAttributes.toString)
        assert(!has(span, "gen_ai.input.messages"))
      }
    }
  }

  test("the trace context is sent to the API and matches the span") {
    env(OtelConfig.default, json(Replies.text("ok"))) { e =>
      for {
        _     <- e.sdk.messages.create(request)
        spans <- e.spans
        sent  <- e.seen.get
      } yield {
        val traceparent = sent.head.headers.get(CIString("traceparent")).map(_.head.value)
        assertEquals(
          traceparent.map(_.split("-").toList.slice(1, 3)),
          Some(List(spans.head.getTraceId, spans.head.getSpanId))
        )
      }
    }
  }

  test("retries stay inside one span and are counted") {
    env(OtelConfig.default, error(529, "overloaded_error"), json(Replies.text("ok"))) { e =>
      for {
        _     <- e.sdk.messages.create(request)
        spans <- e.spans
      } yield {
        assertEquals(spans.size, 1)
        assertEquals(long(spans.head, "http.request.resend_count"), Some(1L))
        assertEquals(long(spans.head, "http.response.status_code"), Some(200L))
      }
    }
  }

  test("a failed request ends its span as an error with the API error type") {
    env(OtelConfig.default, error(400, "invalid_request_error")) { e =>
      for {
        _     <- e.sdk.messages.create(request).attempt
        spans <- e.spans
      } yield {
        assertEquals(spans.head.getStatus.getStatusCode, StatusCode.ERROR)
        assertEquals(str(spans.head, "error.type"), Some("invalid_request_error"))
      }
    }
  }

  test("a stream's span ends when the stream does, with usage and time to first chunk") {
    env(OtelConfig.default, sse(Replies.text("Hello"))) { e =>
      for {
        open  <- e.sdk.messages.stream(request).evalMap(_ => e.spans.map(_.size)).compile.toList
        spans <- e.spans
      } yield {
        assert(open.forall(_ == 0), "the span must still be open while events are being delivered")
        val span = spans.head
        assertEquals(span.getName, "anthropic.messages.create")
        assertEquals(
          span.getAttributes.get(AttributeKey.booleanKey("gen_ai.request.stream")),
          java.lang.Boolean.TRUE
        )
        assertEquals(long(span, "gen_ai.usage.output_tokens"), Some(5L))
        assert(has(span, "gen_ai.response.time_to_first_chunk"))
      }
    }
  }

  test("content mode records prompts and replies, cut to the configured size") {
    env(OtelConfig.default.withContentMode(ContentMode.Content), json(Replies.text("Secret answer"))) { e =>
      for {
        _     <- e.sdk.messages.create(request)
        spans <- e.spans
      } yield {
        assert(str(spans.head, "gen_ai.input.messages").exists(_.contains("Secret question")))
        assert(str(spans.head, "gen_ai.system_instructions").exists(_.contains("Secret instructions")))
        assert(str(spans.head, "gen_ai.output.messages").exists(_.contains("Secret answer")))
      }
    } *> env(
      OtelConfig.default.withContentMode(ContentMode.Content).withMaxContentBytes(8),
      json(Replies.text("x"))
    ) { e =>
      for {
        _     <- e.sdk.messages.create(request)
        spans <- e.spans
      } yield {
        assertEquals(str(spans.head, "gen_ai.input.messages").map(_.length), Some(8))
        assertEquals(
          spans.head.getAttributes.get(AttributeKey.booleanKey("anthropic.content.truncated")),
          java.lang.Boolean.TRUE
        )
      }
    }
  }

  test("a span you open around a call becomes the parent of the request span") {
    env(OtelConfig.default, json(Replies.text("ok"))) { e =>
      for {
        _     <- e.tracer.span("handle-order").surround(e.sdk.messages.create(request))
        spans <- e.spans
      } yield {
        val byName = spans.map(s => s.getName -> s).toMap
        assertEquals(byName("anthropic.messages.create").getParentSpanId, byName("handle-order").getSpanId)
      }
    }
  }

  test("a tool run is one tree: runner, its requests, each tool call, and spans the tool opens") {
    val firstTurn = Replies.toolUse("lookup", Json.obj("city" -> "Brussels".asJson))
    env(OtelConfig.default, sse(firstTurn), sse(Replies.text("Sunny"))) { e =>
      val tool = Tool[IO, Lookup, String]("lookup", "Looks a city up")(in =>
        e.tracer.span("db-query").surround(IO.pure(s"sunny in ${in.city}"))
      )
      for {
        result <- e.sdk.messages.toolRunner(List(tool)).run(request)
        spans  <- e.spans
      } yield {
        assertEquals(result.text, "Sunny")
        val runner   = spans.find(_.getName == "anthropic.messages.tool_runner").get
        val requests = spans.filter(_.getName == "anthropic.messages.create")
        val toolSpan = spans.find(_.getName == "anthropic.custom_tool_use lookup").get
        val inner    = spans.find(_.getName == "db-query").get

        assertEquals(runner.getKind, SpanKind.INTERNAL)
        assertEquals(str(runner, "gen_ai.operation.name"), Some("invoke_agent"))
        assertEquals(requests.size, 2)
        assert(requests.forall(_.getParentSpanId == runner.getSpanId))
        assertEquals(toolSpan.getParentSpanId, runner.getSpanId)
        assertEquals(str(toolSpan, "gen_ai.tool.call.id"), Some("toolu_1"))
        assertEquals(inner.getParentSpanId, toolSpan.getSpanId)
        assertEquals(spans.map(_.getTraceId).distinct.size, 1)
      }
    }
  }

  test("finalMessage on a traced stream still returns the reply") {
    env(OtelConfig.default, sse(Replies.text("Hello"))) { e =>
      e.sdk.messages.stream(request).finalMessage.map(m => assertEquals(m.text, "Hello"))
    }
  }
}
