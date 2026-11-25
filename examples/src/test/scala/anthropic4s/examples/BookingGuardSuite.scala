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
import anthropic4s.testkit.{Replies, ScriptedModel}
import cats.effect.{IO, Ref}
import io.circe.Json
import io.circe.syntax._
import munit.CatsEffectSuite
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.{HttpApp, Response, Status}

/**
 * The booking guard of [[AgentDecisions]], tested through a whole agent run: a scripted Claude calls
 * `book_table`, the real guard decides, and Kev-4B is faked over HTTP. No network, no cost.
 */
class BookingGuardSuite extends CatsEffectSuite {
  import BookingGuardSuite.Outcome

  private val notAsked = "Should I eat outside in Brussels tonight?"
  private val asked    =
    "Should I eat outside in Brussels tonight? If not, book Chez Leon for two on 2026-12-24."

  /** The answer Kev-4B gives to the guard's yes/no question. */
  private def kevSays(p: Double): Response[IO] =
    Response[IO](Status.Ok).withEntity(Json.obj("answers" -> Json.obj("q" -> Json.obj("noul" -> p.asJson))))

  private val kevDown: Response[IO] = Response[IO](Status.ServiceUnavailable).withEntity("loading model")

  /** Claude tries to book, then answers. The booking tool only counts how often it really ran. */
  private def agentRun(userText: String, kev: Response[IO]): IO[Outcome] =
    for {
      bookings <- Ref[IO].of(0)
      kevSaw   <- Ref[IO].of(List.empty[Json])
      decide = Decide.fromClient(
        Client.fromHttpApp(HttpApp[IO](req => req.as[Json].flatMap(b => kevSaw.update(_ :+ b)).as(kev))),
        DecideConfig.local()
      )
      bookTable = Tool("book_table", "Books a restaurant table.")((_: BookTable) =>
        bookings.update(_ + 1).as("Booked.")
      )
      claude <- ScriptedModel.of[IO](
        Replies.toolUse(
          "book_table",
          Json.obj(
            "restaurant" -> "Chez Leon".asJson,
            "date"       -> "2026-12-24".asJson,
            "guests"     -> 2.asJson
          )
        ),
        Replies.text("Done.")
      )
      config = RunnerConfig.default[IO].withBeforeTool(AgentDecisions.bookingGuard(decide, userText))
      result <- claude
        .toolRunner(List(bookTable), config)
        .run(MessageRequest(Model.ClaudeOpus5_5, 1024, MessageParam.user(userText)))
      sent  <- claude.requests
      count <- bookings.get
      seen  <- kevSaw.get
    } yield {
      // The second request to Claude carries the result of its tool call.
      val toolResult = sent(1).messages.last.content.blocks.collectFirst { case r: ContentBlock.ToolResult =>
        r
      }
      Outcome(count, toolResult.getOrElse(fail("no tool result went back to Claude")), result.text, seen)
    }

  test("an unrequested booking is denied, never runs, and Claude is told why") {
    agentRun(notAsked, kevSays(0.11)).map { o =>
      assertEquals(o.bookings, 0)
      assert(o.toolResult.isError)
      assert(o.toolResult.text.contains("did not ask for a booking"), o.toolResult.text)
      // A deny does not end the run: Claude reads the refusal and carries on.
      assertEquals(o.reply, "Done.")
    }
  }

  test("the guard shows Kev-4B the user's words as a conversation") {
    agentRun(notAsked, kevSays(0.11)).map { o =>
      assertEquals(o.kevSaw.size, 1)
      assertEquals(o.kevSaw.head.hcursor.get[Json]("state"), Right(AgentDecisions.conversation(notAsked)))
    }
  }

  test("a requested booking runs") {
    agentRun(asked, kevSays(0.70)).map { o =>
      assertEquals(o.bookings, 1)
      assert(!o.toolResult.isError)
      assertEquals(o.toolResult.text, "Booked.")
    }
  }

  test("it fails closed: if Kev-4B is down, nothing is booked") {
    agentRun(asked, kevDown).map { o =>
      assertEquals(o.bookings, 0)
      assert(o.toolResult.isError)
      assert(o.toolResult.text.contains("cannot be checked"), o.toolResult.text)
    }
  }
}

object BookingGuardSuite {

  /** What one agent run did: how often the booking ran, what went back to Claude, what Kev-4B was asked. */
  final case class Outcome(
      bookings: Int,
      toolResult: ContentBlock.ToolResult,
      reply: String,
      kevSaw: List[Json]
  )
}
