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

/** One option of a choice: the value you get back, the label the model sees, and an optional description. */
final case class Criterion[A](value: A, label: String, description: Option[String])

object Criterion {
  def apply[A](value: A, label: String, description: String): Criterion[A] =
    Criterion(value, label, Some(description))

  /** An option with no description: the model only sees the label. */
  def bare[A](value: A, label: String): Criterion[A] = Criterion(value, label, None)
}

final case class ChoiceAnswer[A](choice: A, probabilities: Map[A, Double], confidence: Double)

final case class ScoreAnswer(
    score: Double,
    levels: Vector[String],
    probabilities: Vector[Double],
    confidence: Double
) {

  /** The level closest to the probability-weighted score. */
  def nearest: String = levels(math.round(score).toInt.max(0).min(levels.size - 1))

  /** The single most probable level. It can differ from `nearest` when the mass is spread out. */
  def mostLikely: String = levels(probabilities.indices.maxBy(probabilities))
}
