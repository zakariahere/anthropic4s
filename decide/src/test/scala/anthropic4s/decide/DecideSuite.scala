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

import cats.effect.{IO, Ref, Resource}
import cats.syntax.all._
import io.circe.Json
import io.circe.syntax._
import munit.CatsEffectSuite
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.headers.Authorization
import org.http4s.{AuthScheme, Credentials, Headers, HttpRoutes, Response}

import java.net.ConnectException
import scala.concurrent.duration._

class DecideSuite extends CatsEffectSuite {
  import DecideSuite.Seen

  sealed trait Route
  object Route {
    case object Billing   extends Route
    case object Shipping  extends Route
    case object Technical extends Route
  }

  private val route = Question.choice[Route]("Which team should handle this?")(
    Criterion(Route.Billing, "billing", "payments, charges, refunds, invoices"),
    Criterion(Route.Shipping, "shipping", "delivery, tracking, lost or late parcels"),
    Criterion(Route.Technical, "technical", "bugs, errors, login problems")
  )
  private val angry   = Question.noul("Is the customer angry?")
  private val urgency = Question.score("How urgent is this?")("can wait", "this week", "today", "right now")

  // A real response, captured from llama-server serving Kev-4B (Q8_0) for the support-ticket batch.
  // Note the score: its probabilities are an object keyed by level index, not an array.
  private val answers = io.circe.jawn
    .parse(
      """{
        |  "model": "ggml-org/Kev-4B-GGUF:Q8_0",
        |  "answers": {
        |    "route": {
        |      "type": "choice",
        |      "choice": "billing",
        |      "probabilities": {
        |        "billing": 0.9129698710886464,
        |        "shipping": 0.02904999673598076,
        |        "technical": 0.05798013217537279
        |      },
        |      "confidence": 0.8694548066329694
        |    },
        |    "angry": { "type": "noul", "noul": 0.8271380103954467 },
        |    "urgency": {
        |      "type": "score",
        |      "score": 2.287975163933644,
        |      "legend": { "0": "can wait", "1": "this week", "2": "today", "3": "right now" },
        |      "probabilities": {
        |        "0": 0.040603234856414684,
        |        "1": 0.19077372408116394,
        |        "2": 0.20866768333478394,
        |        "3": 0.5599553577276374
        |      },
        |      "confidence": 0.2879751639336442
        |    }
        |  },
        |  "usage": { "input_tokens": 130, "output_tokens": 0 }
        |}""".stripMargin
    )
    .fold(throw _, identity)

  /** A fake server that records every request it gets and answers with the captured response. */
  private def fake(config: DecideConfig = DecideConfig.local()): IO[(Decide[IO], Ref[IO, List[Seen]])] =
    Ref[IO].of(List.empty[Seen]).map { seen =>
      val routes = HttpRoutes.of[IO] { case req @ POST -> Root / "v1" / "systemone" =>
        req.as[Json].flatMap(body => seen.update(_ :+ Seen(req.headers, body))) *> Ok(answers)
      }
      (Decide.fromClient(Client.fromHttpApp(routes.orNotFound), config), seen)
    }

  private def answering(routes: HttpRoutes[IO]): Decide[IO] =
    Decide.fromClient(Client.fromHttpApp(routes.orNotFound), DecideConfig.local())

  test("a batch decodes into typed answers") {
    fake().flatMap { case (decide, _) =>
      decide.run("msg", (route.as("route"), angry.as("angry"), urgency.as("urgency")).tupled).map {
        case (r, a, u) =>
          assertEquals(r.choice, Route.Billing)
          assertEquals(r.probabilities(Route.Shipping), 0.02904999673598076)
          assertEquals(r.confidence, 0.8694548066329694)
          assertEquals(a, 0.8271380103954467)
          // Indexed back into level order, lowest first.
          assertEquals(u.probabilities.map(p => math.round(p * 100)), Vector(4L, 19L, 21L, 56L))
          // 2.29 rounds to "today", but "right now" holds the most probability: a spread-out score.
          assertEquals(u.nearest, "today")
          assertEquals(u.mostLikely, "right now")
      }
    }
  }

  test("a score answer missing a level is a decode error") {
    val partial = Json.obj(
      "answers" -> Json.obj(
        "q" -> Json.obj(
          "score"         -> 1.0.asJson,
          "probabilities" -> Json.obj("0" -> 0.5.asJson, "2" -> 0.5.asJson),
          "confidence"    -> 0.1.asJson
        )
      )
    )
    val decide = answering(HttpRoutes.of[IO] { case _ => Ok(partial) })
    interceptIO[DecideError.Decode](decide.ask("msg", urgency)).map { e =>
      assert(e.detail.contains("no probability for level 1"), e.detail)
    }
  }

  test("options and levels are sent in the order they were written") {
    fake().flatMap { case (decide, seen) =>
      decide.run("msg", (route.as("route"), urgency.as("urgency")).tupled) *>
        seen.get.map(_.head.body).map { body =>
          val qs = body.hcursor.downField("questions")
          assertEquals(
            qs.downField("route").downField("criteria").keys.map(_.toList),
            Some(List("billing", "shipping", "technical"))
          )
          assertEquals(
            qs.downField("urgency").get[List[String]]("criteria"),
            Right(List("can wait", "this week", "today", "right now"))
          )
          assertEquals(body.hcursor.get[String]("state"), Right("msg"))
        }
    }
  }

  test("a noul sends its criteria only once they are described") {
    val described = angry.describe("the customer is upset or hostile", "the customer is calm")
    assertEquals(angry.toJson.hcursor.downField("criteria").focus, None)
    assertEquals(
      described.toJson.hcursor.downField("criteria").get[String]("true"),
      Right("the customer is upset or hostile")
    )
  }

  test("the api key is sent as a bearer token") {
    fake(DecideConfig.local().withApiKey("secret")).flatMap { case (decide, seen) =>
      decide.run("msg", angry.as("angry")) *> seen.get.map { requests =>
        assertEquals(
          requests.head.headers.get[Authorization],
          Some(Authorization(Credentials.Token(AuthScheme.Bearer, "secret")))
        )
      }
    }
  }

  test("the config does not print the api key") {
    assert(!DecideConfig.local().withApiKey("secret").toString.contains("secret"))
  }

  test("duplicate ids fail before anything is sent") {
    fake().flatMap { case (decide, seen) =>
      interceptIO[DecideError.InvalidQuestion](decide.run("msg", (angry.as("x"), angry.as("x")).tupled)) *>
        seen.get.map(requests => assertEquals(requests, Nil))
    }
  }

  test("a score with too many levels fails before anything is sent") {
    val tooFine = Question.Score("How many stars?", (1 to 11).map(_.toString).toVector)
    fake().flatMap { case (decide, seen) =>
      interceptIO[DecideError.InvalidQuestion](decide.ask("msg", tooFine)) *>
        seen.get.map(requests => assertEquals(requests, Nil))
    }
  }

  test("an error status becomes DecideError.Http") {
    val decide = answering(HttpRoutes.of[IO] { case _ => ServiceUnavailable("loading model") })
    interceptIO[DecideError.Http](decide.ask("msg", angry)).map { e =>
      assertEquals(e.status, 503)
      assertEquals(e.body, "loading model")
    }
  }

  test("an unknown option label is a decode error") {
    val odd    = Json.obj("answers" -> Json.obj("q" -> Json.obj("choice" -> "refunds".asJson)))
    val decide = answering(HttpRoutes.of[IO] { case _ => Ok(odd) })
    interceptIO[DecideError.Decode](decide.ask("msg", route))
  }

  test("a missing answer is a decode error") {
    val decide = answering(HttpRoutes.of[IO] { case _ => Ok(Json.obj("answers" -> Json.obj())) })
    interceptIO[DecideError.Decode](decide.ask("msg", angry))
  }

  test("a server that cannot be reached becomes DecideError.Unavailable, keeping the cause") {
    val refused =
      Client[IO](_ => Resource.raiseError[IO, Response[IO], Throwable](new ConnectException("refused")))
    interceptIO[DecideError.Unavailable](Decide.fromClient(refused, DecideConfig.local()).ask("msg", angry))
      .map { e =>
        assertEquals(e.endpoint, "http://localhost:8081/v1/systemone")
        assert(e.getCause.isInstanceOf[ConnectException], e.getCause)
      }
  }

  test("a slow server becomes DecideError.Timeout") {
    val slow   = HttpRoutes.of[IO] { case _ => IO.sleep(1.second) *> Ok(answers) }
    val config = DecideConfig.local().withTimeout(50.millis)
    val decide = Decide.fromClient(Client.fromHttpApp(slow.orNotFound), config)
    interceptIO[DecideError.Timeout](decide.ask("msg", angry))
  }
}

object DecideSuite {

  /** What the fake server received. The body is read on arrival: a request body can be read only once. */
  final case class Seen(headers: Headers, body: Json)
}
