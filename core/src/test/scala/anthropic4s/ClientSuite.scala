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

package anthropic4s

import anthropic4s.FakeApi._
import anthropic4s.syntax._
import cats.effect.IO
import cats.effect.testkit.TestControl
import io.circe.parser.parse
import io.circe.syntax._
import munit.CatsEffectSuite

import scala.concurrent.duration._

sealed trait Sky
object Sky {
  case object Sunny  extends Sky
  case object Cloudy extends Sky
}
final case class Weather(city: String, celsius: Double, sky: Sky)

class ClientSuite extends CatsEffectSuite {

  test("create sends the documented headers and body, and decodes the reply") {
    for {
      fake <- scripted(json(200, messageJson, "request-id" -> "req_42"))
      sdk = fake.sdk(AnthropicConfig("sk-test").withBetas(Beta.TaskBudgets))
      raw <- sdk.messages.createRaw(
        request,
        RequestOptions.default.withBetas(Beta.Compaction).withHeader("x-team", "a")
      )
      recorded <- fake.recorded
    } yield {
      val sent = recorded.head
      assertEquals(sent.method, "POST")
      assertEquals(sent.path, "/v1/messages")
      assertEquals(sent.headers.get("x-api-key"), Some("sk-test"))
      assertEquals(sent.headers.get("anthropic-version"), Some("2026-10-21"))
      assertEquals(sent.headers.get("anthropic-beta"), Some("compact-2026-01-12,task-budgets-2026-03-13"))
      assertEquals(sent.headers.get("x-team"), Some("a"))
      assertEquals(parse(sent.body), Right(request.asJson))
      assertEquals(raw.value.text, "Hello!")
      assertEquals(raw.requestId, Some("req_42"))
      assertEquals(raw.status, 200)
    }
  }

  test("an OAuth token is sent as a bearer token") {
    for {
      fake     <- scripted(json(200, messageJson))
      _        <- fake.sdk(AnthropicConfig(Credentials.AuthToken(Secret("tok")))).messages.create(request)
      recorded <- fake.recorded
    } yield {
      assertEquals(recorded.head.headers.get("authorization"), Some("Bearer tok"))
      assertEquals(recorded.head.headers.get("x-api-key"), None)
    }
  }

  test("error statuses become typed errors carrying the request id") {
    for {
      fake <- scripted(error(400, "invalid_request_error", "max_tokens: too large", "request-id" -> "req_7"))
      err  <- fake.sdk().messages.create(request).attempt
      n    <- fake.recorded.map(_.size)
    } yield err match {
      case Left(e: AnthropicError.BadRequest) =>
        assertEquals(e.errorType, "invalid_request_error")
        assertEquals(e.errorMessage, "max_tokens: too large")
        assertEquals(e.requestId, Some("req_7"))
        assertEquals(n, 1) // a 400 is never retried
      case other => fail(s"expected BadRequest, got $other")
    }
  }

  test("overloaded responses are retried with backoff, then succeed") {
    TestControl.executeEmbed {
      for {
        fake <- scripted(
          error(529, "overloaded_error", "Overloaded"),
          error(529, "overloaded_error", "Overloaded"),
          json(200, messageJson)
        )
        start <- IO.monotonic
        reply <- fake.sdk().messages.create(request)
        end   <- IO.monotonic
        n     <- fake.recorded.map(_.size)
      } yield {
        assertEquals(reply.text, "Hello!")
        assertEquals(n, 3)
        // 0.5s then 1s, each shortened by up to 25% jitter.
        val waited = end - start
        assert(waited >= 1125.millis && waited <= 1500.millis, s"waited $waited")
      }
    }
  }

  test("retries stop at the limit and raise the last error, with the server's retry-after hint") {
    TestControl.executeEmbed {
      for {
        fake  <- scripted(error(429, "rate_limit_error", "Slow down", "retry-after" -> "3"))
        start <- IO.monotonic
        err   <- fake.sdk().messages.create(request).attempt
        end   <- IO.monotonic
        n     <- fake.recorded.map(_.size)
      } yield err match {
        case Left(e: AnthropicError.RateLimited) =>
          assertEquals(n, 3) // 1 attempt + 2 retries
          assertEquals(e.retryAfter, Some(3.seconds))
          assertEquals(end - start, 6.seconds) // retry-after replaced the backoff, twice
        case other => fail(s"expected RateLimited, got $other")
      }
    }
  }

  test("x-should-retry: false overrides the default for a 5xx") {
    for {
      fake <- scripted(error(500, "api_error", "boom", "x-should-retry" -> "false"))
      err  <- fake.sdk().messages.create(request).attempt
      n    <- fake.recorded.map(_.size)
    } yield {
      assert(err.left.exists(_.isInstanceOf[AnthropicError.Server]))
      assertEquals(n, 1)
    }
  }

  test("a request that gets no answer times out, and the timeout is retried") {
    TestControl.executeEmbed {
      for {
        fake  <- FakeApi((_, _) => IO.never)
        start <- IO.monotonic
        err   <- fake
          .sdk()
          .messages
          .create(request, RequestOptions.default.withTimeout(5.seconds).withMaxRetries(1))
          .attempt
        end <- IO.monotonic
        n   <- fake.recorded.map(_.size)
      } yield err match {
        case Left(e: AnthropicError.Timeout) =>
          assertEquals(e.after, 5.seconds)
          assertEquals(n, 2)
          assert(end - start >= 10.seconds)
        case other => fail(s"expected Timeout, got $other")
      }
    }
  }

  test("a huge non-streaming request is refused before anything is sent") {
    for {
      fake <- scripted(json(200, messageJson))
      err  <- fake.sdk().messages.create(request.withMaxTokens(128000)).attempt
      n    <- fake.recorded.map(_.size)
    } yield {
      assert(err.left.exists(_.isInstanceOf[AnthropicError.StreamingRequired]))
      assertEquals(n, 0)
    }
  }

  test("a response that is not the expected JSON is a decoding error, not a retry") {
    for {
      fake <- scripted(json(200, """{"unexpected":true}"""))
      err  <- fake.sdk().messages.create(request).attempt
      n    <- fake.recorded.map(_.size)
    } yield {
      assert(err.left.exists(_.isInstanceOf[AnthropicError.Decoding]))
      assertEquals(n, 1)
    }
  }

  test("stream emits events, skips pings and asks the API to stream") {
    for {
      fake     <- scripted(sse(helloStream: _*))
      events   <- fake.sdk().messages.stream(request).compile.toList
      recorded <- fake.recorded
    } yield {
      assertEquals(events.size, helloStream.size - 1) // the ping is dropped
      assertEquals(events.last, StreamEvent.MessageStop)
      assertEquals(parse(recorded.head.body).flatMap(_.hcursor.get[Boolean]("stream")), Right(true))
    }
  }

  test("textDeltas and finalMessage") {
    for {
      fake  <- scripted(sse(helloStream: _*))
      text  <- fake.sdk().messages.stream(request).textDeltas.compile.toList
      reply <- fake.sdk().messages.stream(request).finalMessage
    } yield {
      assertEquals(text, List("Hel", "lo!"))
      assertEquals(reply.text, "Hello!")
      assertEquals(reply.usage.outputTokens, 3L)
      assertEquals(reply.stopReason, Some(StopReason.EndTurn))
    }
  }

  test("an error event fails the stream") {
    val failing = helloStream.take(
      4
    ) :+ ("error" -> """{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}""")
    for {
      fake <- scripted(sse(failing: _*))
      err  <- fake.sdk().messages.stream(request).compile.toList.attempt
    } yield err match {
      case Left(e: AnthropicError.StreamFailure) => assertEquals(e.errorType, "overloaded_error")
      case other                                 => fail(s"expected StreamFailure, got $other")
    }
  }

  test("a stream cut short is reported, but stopping early on purpose is fine") {
    for {
      fake  <- scripted(sse(helloStream.dropRight(1): _*))
      err   <- fake.sdk().messages.stream(request).compile.toList.attempt
      taken <- fake.sdk().messages.stream(request).take(2).compile.toList
    } yield {
      assert(err.left.exists(_.isInstanceOf[AnthropicError.StreamTruncated]))
      assertEquals(taken.size, 2)
    }
  }

  test("a stream is retried while no response has started, and errors after that are not") {
    TestControl.executeEmbed {
      for {
        fake  <- scripted(error(529, "overloaded_error", "Overloaded"), sse(helloStream: _*))
        reply <- fake.sdk().messages.stream(request).finalMessage
        n     <- fake.recorded.map(_.size)
      } yield {
        assertEquals(reply.text, "Hello!")
        assertEquals(n, 2)
      }
    }
  }

  test("count_tokens") {
    for {
      fake     <- scripted(json(200, """{"input_tokens":2095}"""))
      count    <- fake.sdk().messages.countTokens(request)
      recorded <- fake.recorded
    } yield {
      assertEquals(count.inputTokens, 2095L)
      assertEquals(recorded.head.path, "/v1/messages/count_tokens")
      assert(!recorded.head.body.contains("max_tokens"))
    }
  }

  test("models.listAll follows pages") {
    val page1 =
      """{"data":[{"type":"model","id":"claude-opus-5-5","display_name":"Claude Opus 5.5","created_at":"2026-09-01T00:00:00Z","max_input_tokens":1000000,"max_tokens":128000}],"has_more":true,"first_id":"claude-opus-5-5","last_id":"claude-opus-5-5"}"""
    val page2 =
      """{"data":[{"type":"model","id":"claude-haiku-5-5","display_name":"Claude Haiku 5.5","created_at":"2026-09-01T00:00:00+00:00"}],"has_more":false,"first_id":"claude-haiku-5-5","last_id":"claude-haiku-5-5"}"""
    for {
      fake     <- scripted(json(200, page1), json(200, page2))
      models   <- fake.sdk().models.listAll.compile.toList
      recorded <- fake.recorded
    } yield {
      assertEquals(models.map(_.id), List(Model.ClaudeOpus5_5, Model.ClaudeHaiku5_5))
      assertEquals(models.head.maxInputTokens, Some(1000000L))
      assertEquals(recorded(1).query.get("after_id"), Some("claude-opus-5-5"))
    }
  }

  test("a custom base URL with a path prefix is respected") {
    for {
      fake <- scripted(json(200, messageJson))
      _    <- fake
        .sdk(AnthropicConfig("k").withBaseUrl("https://gateway.example.com/anthropic/"))
        .messages
        .create(request)
      recorded <- fake.recorded
    } yield assertEquals(recorded.head.path, "/anthropic/v1/messages")
  }

  test("parse sends the derived schema and decodes the reply into the case class") {
    val answer = """{"city":"Brussels","celsius":21.5,"sky":"Cloudy"}"""
    val reply  = parse(messageJson)
      .map(
        _.hcursor
          .downField("content")
          .downN(0)
          .downField("text")
          .withFocus(_ => answer.asJson)
          .top
          .get
          .noSpaces
      )
      .fold(throw _, identity)
    for {
      fake     <- scripted(json(200, reply))
      parsed   <- fake.sdk().messages.parse[Weather](request)
      recorded <- fake.recorded
    } yield {
      assertEquals(parsed.value, Weather("Brussels", 21.5, Sky.Cloudy))
      assertEquals(parsed.message.usage.outputTokens, 3L)
      val format = parse(recorded.head.body)
        .flatMap(_.hcursor.downField("output_config").downField("format").as[io.circe.Json])
      assertEquals(format.flatMap(_.hcursor.get[String]("type")), Right("json_schema"))
      assertEquals(
        format.flatMap(_.hcursor.downField("schema").get[List[String]]("required")),
        Right(List("city", "celsius", "sky"))
      )
    }
  }

  test("parse reports a refusal, a cut-off reply and a schema that cannot be strict") {
    val refused = messageJson.replace(
      """"stop_reason":"end_turn"""",
      """"stop_reason":"refusal","stop_details":{"type":"refusal","category":"cyber"}"""
    )
    val cutOff = messageJson.replace(""""stop_reason":"end_turn"""", """"stop_reason":"max_tokens"""")
    for {
      fake <- scripted(json(200, refused), json(200, cutOff))
      a    <- fake.sdk().messages.parse[Weather](request).attempt
      b    <- fake.sdk().messages.parse[Weather](request).attempt
      c    <- fake.sdk().messages.parse[List[Weather]](request).attempt
      n    <- fake.recorded.map(_.size)
    } yield {
      assert(a.left.exists {
        case e: AnthropicError.OutputParse => e.detail.contains("cyber"); case _ => false
      })
      assert(b.left.exists {
        case e: AnthropicError.OutputParse => e.detail.contains("max_tokens"); case _ => false
      })
      assert(c.left.exists(_.isInstanceOf[AnthropicError.InvalidSchema]))
      assertEquals(n, 2) // the invalid schema never reached the API
    }
  }

  private val batchJson =
    """{"id":"msgbatch_1","type":"message_batch","processing_status":"STATUS",
      |"request_counts":{"processing":1,"succeeded":0,"errored":0,"canceled":0,"expired":0},
      |"created_at":"2026-11-22T10:00:00Z","expires_at":"2026-11-23T10:00:00Z","ended_at":null,"results_url":null}""".stripMargin

  test("batches: create sends every request with its custom id, and awaitEnded polls until the batch ends") {
    TestControl.executeEmbed {
      for {
        fake <- scripted(
          json(200, batchJson.replace("STATUS", "in_progress")),
          json(200, batchJson.replace("STATUS", "in_progress")),
          json(200, batchJson.replace("STATUS", "ended"))
        )
        sdk = fake.sdk()
        created <- sdk.batches.create(
          cats.data.NonEmptyList.of(BatchRequest("a", request), BatchRequest("b", request))
        )
        start    <- IO.monotonic
        ended    <- sdk.batches.awaitEnded(created.id, 30.seconds)
        end      <- IO.monotonic
        recorded <- fake.recorded
      } yield {
        assertEquals(created.processingStatus, BatchStatus.InProgress)
        assert(ended.hasEnded)
        assertEquals(end - start, 30.seconds) // one poll saw in_progress, the next saw ended
        assertEquals(recorded.head.path, "/v1/messages/batches")
        val ids = parse(recorded.head.body)
          .flatMap(_.hcursor.downField("requests").as[List[io.circe.Json]])
          .map(_.flatMap(_.hcursor.get[String]("custom_id").toOption))
        assertEquals(ids, Right(List("a", "b")))
        assertEquals(recorded(1).path, "/v1/messages/batches/msgbatch_1")
      }
    }
  }

  test("batches: results stream one decoded outcome per line") {
    val lines =
      s"""{"custom_id":"a","result":{"type":"succeeded","message":${messageJson.linesIterator.mkString}}}
         |{"custom_id":"b","result":{"type":"errored","error":{"type":"error","error":{"type":"invalid_request_error","message":"bad"}}}}
         |{"custom_id":"c","result":{"type":"expired"}}
         |""".stripMargin
    for {
      fake    <- scripted(json(200, lines))
      results <- fake.sdk().batches.results("msgbatch_1").compile.toList
      path    <- fake.recorded.map(_.head.path)
    } yield {
      assertEquals(path, "/v1/messages/batches/msgbatch_1/results")
      assertEquals(results.map(_.customId), List("a", "b", "c"))
      assert(results(0).outcome match {
        case BatchOutcome.Succeeded(m) => m.text == "Hello!"; case _ => false
      })
      assert(results(1).outcome match {
        case BatchOutcome.Errored(e) => e.errorType == "invalid_request_error"; case _ => false
      })
      assertEquals(results(2).outcome, BatchOutcome.Expired)
    }
  }

  test("files: upload sends multipart form data, download returns the bytes") {
    val meta =
      """{"id":"file_1","type":"file","filename":"notes.txt","mime_type":"text/plain","size_bytes":5,"created_at":"2026-11-22T10:00:00Z","downloadable":false}"""
    for {
      fake <- scripted(
        json(200, meta),
        json(200, "hello"),
        json(200, """{"id":"file_1","type":"file_deleted"}""")
      )
      sdk = fake.sdk()
      file <- sdk.files.upload(
        "notes.txt",
        "text/plain",
        fs2.Stream.emits("hello".getBytes.toList).covary[IO]
      )
      bytes    <- sdk.files.download(file.id).compile.to(Array)
      _        <- sdk.files.delete(file.id)
      recorded <- fake.recorded
    } yield {
      assertEquals(file.id, "file_1")
      assertEquals(file.sizeBytes, 5L)
      val upload = recorded.head
      assertEquals(upload.path, "/v1/files")
      assert(upload.headers.get("content-type").exists(_.startsWith("multipart/form-data; boundary=")))
      assertEquals(
        upload.headers.get("x-api-key"),
        Some("sk-test")
      ) // the multipart headers did not replace ours
      assert(
        upload.body.contains("name=\"file\"") && upload.body.contains("filename=\"notes.txt\"") && upload.body
          .contains("hello")
      )
      assertEquals(new String(bytes), "hello")
      assertEquals(recorded(1).path, "/v1/files/file_1/content")
      assertEquals(recorded(2).method, "DELETE")
    }
  }

  test("secrets never print") {
    val config = AnthropicConfig("sk-very-secret")
    assert(!config.toString.contains("sk-very-secret"))
  }
}
