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
import org.http4s.ember.client.EmberClientBuilder

import scala.concurrent.duration.Duration

sealed trait Team
object Team {
  case object Billing   extends Team
  case object Shipping  extends Team
  case object Technical extends Team
}

/**
 * Routes a support ticket with a small decision model, and asks Claude only when that model is unsure. Needs
 * llama-server serving Kev-4B on port 8081, and `ANTHROPIC_API_KEY` for the escalation path.
 */
object Decisions extends IOApp.Simple {

  val team = Question.choice[Team]("Which team should handle this?")(
    Criterion(Team.Billing, "billing", "payments, charges, refunds, invoices"),
    Criterion(Team.Shipping, "shipping", "delivery, tracking, lost or late parcels"),
    Criterion(Team.Technical, "technical", "bugs, errors, login problems")
  )
  val angry   = Question.noul("Is the customer angry?")
  val urgency = Question.score("How urgent is this?")("can wait", "this week", "today", "right now")

  val ticket = "I was charged twice for my order last week and nobody has replied."

  def run: IO[Unit] =
    // One HTTP client for both models: one connection pool, one place to add middleware.
    EmberClientBuilder
      .default[IO]
      .withTimeout(Duration.Inf) // each client applies its own timeouts
      .withIdleConnectionTime(Duration.Inf)
      .build
      .use { http =>
        val decide = Decide.fromClient(http, DecideConfig.local())

        // One request, three typed answers.
        decide.run(ticket, (team.as("team"), angry.as("angry"), urgency.as("urgency")).tupled).flatMap {
          case (t, a, u) =>
            val summary = IO.println(
              f"team ${t.choice} (confidence ${t.confidence}%.2f), angry $a%.2f, " +
                s"urgency ${u.nearest} (most likely ${u.mostLikely})"
            )
            // Confident enough: act on the cheap answer. Otherwise pay for Claude.
            val next =
              if (t.confidence >= 0.8) IO.println(s"routing to ${t.choice}")
              else
                AnthropicConfig
                  .fromEnv[IO]
                  .flatMap(config => escalate(AnthropicClient.fromClient(http, config)))
            summary *> next
        }
      }

  private def escalate(claude: AnthropicClient[IO]): IO[Unit] = {
    val request = MessageRequest(
      Model.ClaudeHaiku5_5,
      maxTokens = 1024,
      MessageParam.user(s"Which team should handle this ticket: billing, shipping or technical?\n\n$ticket")
    )
    claude.messages.create(request).flatMap(reply => IO.println(s"Claude says: ${reply.text}"))
  }
}
