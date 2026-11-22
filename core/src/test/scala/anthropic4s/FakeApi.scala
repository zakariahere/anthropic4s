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

import anthropic4s.internal.Sse
import cats.effect.{IO, Ref}
import org.http4s.client.Client
import org.http4s.{Header, HttpApp, Request, Response, Status}
import org.typelevel.ci.CIString

/** An in-memory stand-in for the API: scripted responses, recorded requests, no network. */
object FakeApi {

  final case class Recorded(
      method: String,
      path: String,
      query: Map[String, String],
      headers: Map[String, String],
      body: String
  )

  final class Fake(val client: Client[IO], requests: Ref[IO, Vector[Recorded]]) {
    def recorded: IO[Vector[Recorded]]                                                 = requests.get
    def sdk(config: AnthropicConfig = AnthropicConfig("sk-test")): AnthropicClient[IO] =
      AnthropicClient.fromClient[IO](client, config)
  }

  /** `respond` gets the zero-based index of the request and the request itself. */
  def apply(respond: (Int, Request[IO]) => IO[Response[IO]]): IO[Fake] =
    Ref.of[IO, Vector[Recorded]](Vector.empty).map { requests =>
      val app = HttpApp[IO] { req =>
        for {
          body <- req.bodyText.compile.string
          rec = Recorded(
            req.method.name,
            req.uri.path.renderString,
            req.uri.query.params,
            req.headers.headers.map(h => h.name.toString.toLowerCase -> h.value).toMap,
            body
          )
          index    <- requests.modify(rs => (rs :+ rec, rs.size))
          response <- respond(index, req)
        } yield response
      }
      new Fake(Client.fromHttpApp(app), requests)
    }

  /** Answers every request with the next response of the script; the last one repeats. */
  def scripted(responses: Response[IO]*): IO[Fake] =
    apply((i, _) => IO.pure(responses(math.min(i, responses.size - 1))))

  def json(status: Int, body: String, headers: (String, String)*): Response[IO] =
    headers.foldLeft(Response[IO](Status.fromInt(status).fold(throw _, identity)).withEntity(body)) {
      case (r, (k, v)) => r.putHeaders(Header.Raw(CIString(k), v))
    }

  def error(status: Int, errorType: String, message: String, headers: (String, String)*): Response[IO] =
    json(status, s"""{"type":"error","error":{"type":"$errorType","message":"$message"}}""", headers: _*)

  def sse(events: (String, String)*): Response[IO] =
    Response[IO](Status.Ok).withBodyStream(Sse.render(events.toList).covary[IO])

  val messageJson: String =
    """{"id":"msg_1","type":"message","role":"assistant","model":"claude-opus-5-5",
      |"content":[{"type":"text","text":"Hello!"}],"stop_reason":"end_turn","stop_sequence":null,
      |"usage":{"input_tokens":10,"output_tokens":3,"cache_creation_input_tokens":0,"cache_read_input_tokens":0}}""".stripMargin

  val helloStream: List[(String, String)] = List(
    "message_start" -> """{"type":"message_start","message":{"id":"msg_1","type":"message","role":"assistant","model":"claude-opus-5-5","content":[],"stop_reason":null,"usage":{"input_tokens":10,"output_tokens":1}}}""",
    "content_block_start" -> """{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}""",
    "ping" -> """{"type":"ping"}""",
    "content_block_delta" -> """{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Hel"}}""",
    "content_block_delta" -> """{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"lo!"}}""",
    "content_block_stop" -> """{"type":"content_block_stop","index":0}""",
    "message_delta" -> """{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":3}}""",
    "message_stop" -> """{"type":"message_stop"}"""
  )

  val request: MessageRequest = MessageRequest(Model.ClaudeOpus5_5, 1024, MessageParam.user("Hi"))
}
