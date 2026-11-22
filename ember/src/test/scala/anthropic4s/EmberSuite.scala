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

import anthropic4s.syntax._
import cats.effect.{IO, Ref, Resource}
import com.comcast.ip4s._
import fs2.Stream
import munit.CatsEffectSuite
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.{HttpRoutes, Response, Status}

import scala.concurrent.duration._

/** Real sockets on localhost: proves the Ember wiring, SSE framing and connection handling end to end. */
class EmberSuite extends CatsEffectSuite {

  private val message =
    """{"id":"msg_1","type":"message","role":"assistant","model":"claude-opus-5-5",
      |"content":[{"type":"text","text":"Hello!"}],"stop_reason":"end_turn",
      |"usage":{"input_tokens":10,"output_tokens":3}}""".stripMargin

  private def sse(name: String, data: String): String = s"event: $name\ndata: $data\n\n"

  private val frames: List[String] = List(
    sse(
      "message_start",
      """{"type":"message_start","message":{"id":"msg_1","type":"message","role":"assistant","model":"claude-opus-5-5","content":[],"usage":{"input_tokens":10,"output_tokens":1}}}"""
    ),
    sse(
      "content_block_start",
      """{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}"""
    ),
    sse(
      "content_block_delta",
      """{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hel"}}"""
    ),
    sse(
      "content_block_delta",
      """{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"lo!"}}"""
    ),
    sse("content_block_stop", """{"type":"content_block_stop","index":0}"""),
    sse(
      "message_delta",
      """{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":3}}"""
    ),
    sse("message_stop", """{"type":"message_stop"}""")
  )

  /** A local server; `released` counts streaming responses whose body was closed. */
  private def server(released: Ref[IO, Int]): Resource[IO, AnthropicClient[IO]] = {
    val routes = HttpRoutes.of[IO] { case req @ POST -> Root / "v1" / "messages" =>
      req.bodyText.compile.string.flatMap { body =>
        if (body.contains("\"stream\":true"))
          IO.pure(
            Response[IO](Status.Ok).withBodyStream(
              // One frame every 50 ms, then keep-alive comments forever: the client must not depend on
              // the server ending the body, and the server notices a closed socket when a write fails.
              (Stream.emits(frames).covary[IO].metered(50.millis) ++
                Stream.awakeEvery[IO](50.millis).as(": keep-alive\n\n"))
                .through(fs2.text.utf8.encode)
                .onFinalize(released.update(_ + 1))
            )
          )
        else IO.sleep(200.millis) *> Ok(message)
      }
    }
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(port"0")
      .withHttpApp(routes.orNotFound)
      .withShutdownTimeout(1.second)
      .build
      .flatMap { srv =>
        Anthropic4s.resource[IO](
          AnthropicConfig("sk-test").withBaseUrl(s"http://127.0.0.1:${srv.address.getPort}")
        )
      }
  }

  private val request = MessageRequest(Model.ClaudeOpus5_5, 1024, MessageParam.user("Hi"))

  test("create over a real connection") {
    Ref
      .of[IO, Int](0)
      .flatMap(r => server(r).use(_.messages.create(request)))
      .map(m => assertEquals(m.text, "Hello!"))
  }

  test("a stream completes at message_stop even though the server keeps the connection open") {
    Ref.of[IO, Int](0).flatMap { released =>
      server(released).use { client =>
        client.messages
          .stream(request)
          .finalMessage
          .timeout(10.seconds)
          .map(m => assertEquals(m.text, "Hello!"))
      }
    }
  }

  test("abandoning a stream closes its connection") {
    Ref.of[IO, Int](0).flatMap { released =>
      server(released).use { client =>
        for {
          timed <- client.messages.stream(request).take(2).compile.toList.timed
          (took, first) = timed
          // The server notices the closed socket and finalizes its response body.
          _ <- (IO.sleep(50.millis) *> released.get).iterateUntil(_ >= 1).timeout(5.seconds)
        } yield {
          assertEquals(first.size, 2)
          // Without the drain setting, releasing a half-read response takes Ember 5 seconds.
          assert(took < 3.seconds, s"abandoning the stream took $took")
        }
      }
    }
  }
}
