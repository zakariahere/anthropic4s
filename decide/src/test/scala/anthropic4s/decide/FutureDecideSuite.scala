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

package anthropic4s.decide

import cats.effect.IO
import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import munit.FunSuite
import org.http4s.HttpRoutes
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.dsl.io._

import scala.concurrent.{ExecutionContext, Future}

/** Plain `Future` tests, the way a Play application would use the client. */
class FutureDecideSuite extends FunSuite {
  implicit private val ec: ExecutionContext = ExecutionContext.global

  private val angry   = Question.noul("Is the customer angry?")
  private val urgency = Question.score("How urgent is this?")("later", "now")

  private def answering(answers: Json): FutureDecide = {
    val routes = HttpRoutes.of[IO] { case _ => Ok(Json.obj("answers" -> answers)) }
    FutureDecide.wrap(Decide.fromClient(Client.fromHttpApp(routes.orNotFound), DecideConfig.local()), IO.unit)
  }

  private val noul  = Json.obj("type" -> "noul".asJson, "noul" -> 0.83.asJson)
  private val score = Json.obj(
    "type"          -> "score".asJson,
    "score"         -> 0.9.asJson,
    "probabilities" -> Json.obj("0" -> 0.1.asJson, "1" -> 0.9.asJson),
    "confidence"    -> 0.8.asJson
  )

  test("ask completes with the typed answer") {
    answering(Json.obj("q" -> noul)).ask("I want a refund NOW", angry).map(p => assertEquals(p, 0.83))
  }

  test("a batch completes with every answer") {
    answering(Json.obj("angry" -> noul, "urgency" -> score))
      .run("I want a refund NOW", (angry.as("angry"), urgency.as("urgency")).tupled)
      .map { case (a, u) =>
        assertEquals(a, 0.83)
        assertEquals(u.mostLikely, "now")
      }
  }

  test("a failure is a failed future holding a DecideError") {
    val down = FutureDecide.wrap(
      Decide.fromClient(
        Client.fromHttpApp(HttpRoutes.of[IO] { case _ => ServiceUnavailable("loading") }.orNotFound),
        DecideConfig.local()
      ),
      IO.unit
    )
    down
      .ask("msg", angry)
      .map(_ => fail("expected a failure"))
      .recover { case e: DecideError.Http => assertEquals(e.status, 503) }
  }

  test("close runs the shutdown once") {
    var closed = 0
    val decide = FutureDecide.wrap(
      Decide.fromClient(Client.fromHttpApp(HttpRoutes.empty[IO].orNotFound), DecideConfig.local()),
      IO(closed += 1)
    )
    decide.close().flatMap(_ => Future(assertEquals(closed, 1)))
  }
}
