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

import cats.syntax.all._
import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Json}

/**
 * A typed question. `A` is the type of its answer, so `decide.ask(state, question)` returns an `A`. Each
 * question knows how to write itself as JSON and how to read its own answer.
 */
sealed abstract class Question[A] extends Product with Serializable {
  def instructions: String

  /** This question under an id, ready to combine with others into one request. */
  def as(id: String): Batch[A] = Batch.one(id, this)

  private[decide] def toJson: Json
  private[decide] def answerDecoder: Decoder[A]

  /** Why the server would reject this question, checked before anything is sent. */
  private[decide] def problem: Option[String] = None

  protected def json(tpe: String, criteria: Option[Json]): Json =
    Json.fromFields(
      List("type" -> tpe.asJson, "instructions" -> instructions.asJson) ++ criteria.map("criteria" -> _)
    )
}

object Question {

  final case class Choice[A](instructions: String, options: Vector[Criterion[A]])
      extends Question[ChoiceAnswer[A]] {

    // A JSON object keeps insertion order, so the model sees the options in the order you wrote them.
    private[decide] def toJson: Json =
      json("choice", Some(Json.fromFields(options.map(o => o.label -> o.description.asJson))))

    override private[decide] def problem: Option[String] = {
      val labels = options.map(_.label)
      Option.when(labels.distinct.size != labels.size)(s"duplicate option labels in '$instructions'")
    }

    private[decide] def answerDecoder: Decoder[ChoiceAnswer[A]] = Decoder.instance { c =>
      val byLabel                                  = options.map(o => o.label -> o.value).toMap
      def lookup(label: String): Decoder.Result[A] =
        byLabel.get(label).toRight(DecodingFailure(s"unknown option '$label'", c.history))
      for {
        chosen     <- c.get[String]("choice").flatMap(lookup)
        raw        <- c.get[Map[String, Double]]("probabilities")
        probs      <- raw.toList.traverse { case (label, p) => lookup(label).map(_ -> p) }
        confidence <- c.get[Double]("confidence")
      } yield ChoiceAnswer(chosen, probs.toMap, confidence)
    }
  }

  /** Levels go lowest first. They are a `Vector`, never a `Map`: order is the whole meaning of a score. */
  final case class Score(instructions: String, levels: Vector[String]) extends Question[ScoreAnswer] {
    private[decide] def toJson: Json = json("score", Some(levels.asJson))

    override private[decide] def problem: Option[String] =
      Option.when(levels.size < 2 || levels.size > 10)(
        s"a score needs 2 to 10 levels, '$instructions' has ${levels.size}"
      )

    // The server keys the probabilities by level index, as a JSON object: {"0": 0.04, "1": 0.19, ...}.
    private[decide] def answerDecoder: Decoder[ScoreAnswer] = Decoder.instance { c =>
      def inOrder(byIndex: Map[Int, Double]): Decoder.Result[Vector[Double]] =
        levels.indices.toVector.traverse { i =>
          byIndex.get(i).toRight(DecodingFailure(s"no probability for level $i", c.history))
        }
      for {
        score      <- c.get[Double]("score")
        probs      <- c.get[Map[Int, Double]]("probabilities").flatMap(inOrder)
        confidence <- c.get[Double]("confidence")
      } yield ScoreAnswer(score, levels, probs, confidence)
    }
  }

  /** A yes/no question. The answer is the probability that the statement is true. */
  final case class Noul(instructions: String, whenTrue: Option[String], whenFalse: Option[String])
      extends Question[Double] {

    /** Says what true and false mean; in the blog this moved "angry" from 0.828 to 0.835. */
    def describe(whenTrue: String, whenFalse: String): Noul =
      copy(whenTrue = Some(whenTrue), whenFalse = Some(whenFalse))

    private[decide] def toJson: Json = {
      val criteria = (whenTrue, whenFalse) match {
        case (None, None) => None
        case (t, f)       => Some(Json.obj("true" -> t.asJson, "false" -> f.asJson))
      }
      json("noul", criteria)
    }

    private[decide] def answerDecoder: Decoder[Double] = Decoder.instance(_.get[Double]("noul"))
  }

  /** The signature asks for two options, so a one-option choice does not compile. */
  def choice[A](
      instructions: String
  )(first: Criterion[A], second: Criterion[A], more: Criterion[A]*): Choice[A] =
    Choice(instructions, Vector(first, second) ++ more)

  def score(instructions: String)(lowest: String, next: String, higher: String*): Score =
    Score(instructions, Vector(lowest, next) ++ higher)

  def noul(instructions: String): Noul = Noul(instructions, None, None)
}
