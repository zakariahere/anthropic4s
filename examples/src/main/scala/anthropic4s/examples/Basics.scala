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
import anthropic4s.schema.description
import anthropic4s.syntax._
import cats.effect.{IO, IOApp}
import cats.syntax.all._

/** The smallest useful program: one question, one answer. Needs `ANTHROPIC_API_KEY`. */
object Hello extends IOApp.Simple {

  def run: IO[Unit] =
    // `use` opens the client, runs the block, and closes the connections afterwards, even on failure.
    Anthropic4s.fromEnv[IO].use { client =>
      val request = MessageRequest(
        Model.ClaudeOpus5_5,
        maxTokens = 16000,
        MessageParam.user("In one sentence, why do cats purr?")
      )

      client.messages.create(request).flatMap { reply =>
        // Always look at the stop reason before trusting the content.
        if (reply.isRefusal) IO.println("The model declined to answer.")
        else IO.println(reply.text) *> IO.println(reply.usage.show)
      }
    }
}

/** Prints the reply while it is being written, then the token usage. */
object Streaming extends IOApp.Simple {

  def run: IO[Unit] =
    Anthropic4s.fromEnv[IO].use { client =>
      // Streaming has no request timeout to worry about, so the output limit can be generous.
      val request = MessageRequest(
        Model.ClaudeOpus5_5,
        maxTokens = 64000,
        MessageParam.user("Write a haiku about monads, then explain it.")
      )
        .withThinking(ThinkingConfig.adaptive)

      client.messages
        .stream(request)
        .evalTap {
          case StreamEvent.ContentBlockDelta(_, Delta.Text(text)) => IO.print(text)
          case _                                                  => IO.unit
        }
        .finalMessage // the same stream, folded into the complete reply
        .flatMap(reply => IO.println(s"\n\n${reply.usage.show}"))
    }
}

sealed trait Difficulty
object Difficulty {
  case object Easy   extends Difficulty
  case object Medium extends Difficulty
  case object Hard   extends Difficulty
}
final case class Ingredient(name: String, @description("Amount with its unit, e.g. '200 g'") quantity: String)
final case class Recipe(
    name: String,
    @description("Total time in minutes, preparation included") minutes: Int,
    difficulty: Difficulty,
    ingredients: List[Ingredient],
    @description("A wine that goes with it, if any") pairing: Option[String]
)

/** Structured output: the reply arrives as a `Recipe`, not as text to parse. No JSON in sight. */
object StructuredOutput extends IOApp.Simple {

  def run: IO[Unit] =
    Anthropic4s.fromEnv[IO].use { client =>
      val request = MessageRequest(
        Model.ClaudeOpus5_5,
        maxTokens = 16000,
        MessageParam.user("A weeknight vegetarian pasta, please.")
      )

      client.messages
        .parse[Recipe](request)
        .flatMap { parsed =>
          val recipe = parsed.value
          IO.println(s"${recipe.name}: ${recipe.minutes} min, ${recipe.difficulty}") *>
            recipe.ingredients.traverse_(i => IO.println(s"  - ${i.quantity} ${i.name}"))
        }
        .recoverWith { case e: AnthropicError.OutputParse => IO.println(s"No recipe today: ${e.detail}") }
    }
}

/** Typed errors: each failure mode is its own class, so handling is a pattern match. */
object Errors extends IOApp.Simple {

  def run: IO[Unit] =
    Anthropic4s.fromEnv[IO].use { client =>
      val request = MessageRequest(Model.of("claude-does-not-exist"), 1024, MessageParam.user("Hi"))

      client.messages
        .create(request)
        .map(_.text)
        .recover {
          case e: AnthropicError.NotFound    => s"No such model (request ${e.requestId.getOrElse("?")})"
          case e: AnthropicError.RateLimited =>
            s"Slow down; retry in ${e.retryAfter.fold("a while")(_.toString)}"
          case e: AnthropicError.Api     => s"The API said ${e.status} ${e.errorType}: ${e.errorMessage}"
          case _: AnthropicError.Timeout => "Timed out"
        }
        .flatMap(IO.println)
    }
}
