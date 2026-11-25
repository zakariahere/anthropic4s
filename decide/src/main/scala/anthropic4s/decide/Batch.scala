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

import cats.Applicative
import cats.syntax.all._
import io.circe.{Decoder, DecodingFailure, JsonObject}

/**
 * Questions sent together in one request, plus how to read their answers back into an `A`. Batches combine
 * with ordinary cats syntax:
 * {{{
 * val both: Batch[(ChoiceAnswer[Route], Double)] = (route.as("route"), angry.as("angry")).tupled
 * }}}
 */
final class Batch[A] private (
    private[decide] val questions: Vector[(String, Question[_])],
    private[decide] val read: JsonObject => Decoder.Result[A]
)

object Batch {

  def one[A](id: String, question: Question[A]): Batch[A] =
    new Batch[A](
      Vector(id -> question),
      answers =>
        answers(id)
          .toRight(DecodingFailure(s"no answer for '$id'", Nil))
          .flatMap(question.answerDecoder.decodeJson)
    )

  /** Combining two batches sends both sets of questions and reads both sets of answers. */
  implicit val applicative: Applicative[Batch] = new Applicative[Batch] {
    def pure[A](a: A): Batch[A] = new Batch[A](Vector.empty, _ => Right(a))

    def ap[A, B](ff: Batch[A => B])(fa: Batch[A]): Batch[B] =
      new Batch[B](
        ff.questions ++ fa.questions,
        answers => (ff.read(answers), fa.read(answers)).mapN((f, a) => f(a))
      )
  }
}
