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

import anthropic4s.decide._
import cats.syntax.all._
import play.api.libs.json.{JsObject, Json}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * Ticket triage as a Play application would write it: a service built on `Future`, returning JSON. In Play,
 * make it a singleton and let a controller call it:
 * {{{
 * @Singleton
 * class TriageController @Inject() (cc: ControllerComponents, lifecycle: ApplicationLifecycle)(implicit
 *     ec: ExecutionContext
 * ) extends AbstractController(cc) {
 *   private val decide = FutureDecide.fromEnv() // DECIDE_BASE_URL, or llama-server on localhost:8081
 *   lifecycle.addStopHook(() => decide.close())
 *   private val triage = new Triage(decide)
 *
 *   def classify = Action.async(parse.text)(request => triage.classify(request.body).map(Ok(_)))
 * }
 * }}}
 */
final class Triage(decide: FutureDecide)(implicit ec: ExecutionContext) {
  import Decisions.{angry, team, urgency}

  /** Three questions in one request, about 0.1 s warm. A human takes over when the model is unsure. */
  def classify(ticket: String): Future[JsObject] =
    decide
      .run(ticket, (team.as("team"), angry.as("angry"), urgency.as("urgency")).tupled)
      .map { case (t, a, u) =>
        Json.obj(
          "team"       -> t.choice.toString.toLowerCase,
          "confidence" -> t.confidence,
          "angry"      -> (a >= 0.5),
          "urgency"    -> u.mostLikely,
          "needsHuman" -> (t.confidence < 0.8)
        )
      }
      // The decision model is a shortcut, not a dependency: if it is down, a human triages.
      .recover { case e: DecideError => Json.obj("error" -> e.getMessage, "needsHuman" -> true) }
}

/**
 * Classifies a few tickets with [[Triage]]. Needs a System One server: `DECIDE_BASE_URL`, by default
 * llama-server on port 8081.
 */
object FutureDecisions {

  def main(args: Array[String]): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.global

    val decide  = FutureDecide.fromEnv()
    val triage  = new Triage(decide)
    val tickets = List(
      "I was charged twice for my order last week and nobody has replied.",
      "My parcel says delivered but it is not here. Where is it?",
      "The app logs me out every time I open the settings page."
    )

    val all = Future.traverse(tickets)(ticket => triage.classify(ticket).map(ticket -> _))
    Await.result(all, 1.minute).foreach { case (ticket, json) =>
      println(s"$ticket\n  ${Json.stringify(json)}")
    }
    Await.result(decide.close(), 10.seconds)
  }
}
