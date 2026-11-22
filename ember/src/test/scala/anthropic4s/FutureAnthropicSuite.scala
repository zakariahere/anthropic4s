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

import anthropic4s.testkit.{Replies, ScriptedModel}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.Json
import io.circe.syntax._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

final case class Add(a: Int, b: Int)
final case class Sum(total: Int)

/** Plain `Future` code, the way a Play application would call the SDK. No `IO` in the test bodies. */
class FutureAnthropicSuite extends munit.FunSuite {

  private val request = MessageRequest(Model.ClaudeOpus5_5, 1024, MessageParam.user("2 + 3?"))

  private def anthropic(replies: Message*): FutureAnthropic = {
    val model  = ScriptedModel.of[IO](replies: _*).unsafeRunSync()
    val client = new AnthropicClient[IO] {
      val messages: Messages[IO] = model
      def models: Models[IO]     = throw new UnsupportedOperationException
      def batches: Batches[IO]   = throw new UnsupportedOperationException
      def files: Files[IO]       = throw new UnsupportedOperationException
    }
    FutureAnthropic.wrap(client, IO.unit)
  }

  private def await[A](f: Future[A]): A = Await.result(f, 10.seconds)

  test("create and parse return futures") {
    val sdk = anthropic(Replies.text("5"), Replies.structured(Sum(5)))
    assertEquals(await(sdk.create(request)).text, "5")
    assertEquals(await(sdk.parse[Sum](request)).value, Sum(5))
  }

  test("streamText calls back with each piece and completes with the whole reply") {
    val pieces = new java.util.concurrent.ConcurrentLinkedQueue[String]()
    val reply  = await(anthropic(Replies.text("five")).streamText(request) { piece => pieces.add(piece); () })
    assertEquals(reply.text, "five")
    assertEquals(pieces.toArray.toList, List[AnyRef]("five"))
  }

  test("a Future-based tool runs inside the agent loop") {
    import scala.concurrent.ExecutionContext.Implicits.{global => ec}
    val add = FutureAnthropic.tool("add", "Adds two numbers")((in: Add) => Future(Sum(in.a + in.b))(ec))
    val sdk =
      anthropic(Replies.toolUse("add", Json.obj("a" -> 2.asJson, "b" -> 3.asJson)), Replies.text("It is 5."))
    val result = await(sdk.runTools(request, List(add)))
    assertEquals(result.text, "It is 5.")
    assertEquals(result.stop, RunStop.Completed)
  }

  test("errors arrive as failed futures") {
    val sdk = anthropic() // no reply scripted
    assert(await(sdk.create(request).failed).isInstanceOf[IllegalStateException])
  }
}
