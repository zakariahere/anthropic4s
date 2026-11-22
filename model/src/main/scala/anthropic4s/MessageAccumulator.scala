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

import io.circe.syntax._
import io.circe.{Json, JsonObject}

import scala.util.control.NoStackTrace

/** Why a sequence of stream events could not be folded into a message. */
sealed abstract class AccumulatorError(message: String) extends Exception(message) with NoStackTrace
object AccumulatorError {
  case object MissingMessageStart extends AccumulatorError("a stream event arrived before message_start")
  final case class UnknownBlockIndex(index: Int)
      extends AccumulatorError(s"a stream event referred to content block $index, which was never started")
  case object Incomplete extends AccumulatorError("the stream ended before message_stop")
}

/**
 * Rebuilds a [[Message]] from [[StreamEvent]]s. It is a pure, immutable fold:
 * {{{
 * events.foldLeft(MessageAccumulator.empty.asRight[AccumulatorError])((acc, e) => acc.flatMap(_.add(e)))
 *   .flatMap(_.result)
 * }}}
 */
final class MessageAccumulator private (
    private val start: Option[Message],
    private val slots: Vector[MessageAccumulator.Slot],
    private val usage: JsonObject,
    private val stopped: Boolean
) {
  import MessageAccumulator.Slot

  def add(event: StreamEvent): Either[AccumulatorError, MessageAccumulator] = event match {
    case StreamEvent.MessageStart(message) =>
      Right(
        new MessageAccumulator(
          Some(message),
          message.content.map(Slot(_, Vector.empty, None, Vector.empty)).toVector,
          message.usage.asJson.asObject.getOrElse(JsonObject.empty),
          stopped = false
        )
      )

    case _ if start.isEmpty => Left(AccumulatorError.MissingMessageStart)

    case StreamEvent.ContentBlockStart(index, block) =>
      // Blocks arrive in order; padding keeps a skipped index from corrupting later ones.
      val padded = slots.padTo(
        index,
        Slot(ContentBlock.Unknown("missing", JsonObject.empty), Vector.empty, None, Vector.empty)
      )
      Right(copy(slots = padded.take(index) :+ Slot(block, Vector.empty, None, Vector.empty)))

    case StreamEvent.ContentBlockDelta(index, delta) =>
      slots.lift(index).toRight(AccumulatorError.UnknownBlockIndex(index)).map { slot =>
        val next = delta match {
          case Delta.Text(t)      => slot.copy(chunks = slot.chunks :+ t)
          case Delta.Thinking(t)  => slot.copy(chunks = slot.chunks :+ t)
          case Delta.InputJson(p) => slot.copy(chunks = slot.chunks :+ p)
          case Delta.Signature(s) => slot.copy(signature = Some(s))
          case Delta.Citation(c)  => slot.copy(citations = slot.citations :+ c)
          case _: Delta.Unknown   => slot
        }
        copy(slots = slots.updated(index, next))
      }

    case StreamEvent.ContentBlockStop(index) =>
      slots.lift(index).toRight(AccumulatorError.UnknownBlockIndex(index)).map { slot =>
        copy(slots =
          slots.updated(index, Slot(slot.materialize(closed = true), Vector.empty, None, Vector.empty))
        )
      }

    case StreamEvent.MessageDelta(reason, sequence, details, deltaUsage) =>
      // Counters in message_delta are cumulative, so present values replace the ones from message_start.
      val merged = deltaUsage.toIterable.foldLeft(usage) {
        case (acc, (_, v)) if v.isNull => acc
        case (acc, (k, v))             => acc.add(k, v)
      }
      Right(copy(start = start.map(_.withStop(reason, sequence, details)), usage = merged))

    case StreamEvent.MessageStop => Right(copy(stopped = true))

    case _: StreamEvent.Unknown => Right(this)
  }

  /** The message as received so far, or `None` before `message_start`. Costs O(size) per call. */
  def partial: Option[Message] = start.map(build)

  /** The complete message. Fails with [[AccumulatorError.Incomplete]] if `message_stop` was not seen. */
  def result: Either[AccumulatorError, Message] =
    start match {
      case None                => Left(AccumulatorError.MissingMessageStart)
      case Some(_) if !stopped => Left(AccumulatorError.Incomplete)
      case Some(m)             => Right(build(m))
    }

  private def build(m: Message): Message =
    m.withContent(slots.map(_.materialize(closed = stopped)).toList)
      .withUsage(Json.fromJsonObject(usage).as[Usage].getOrElse(m.usage))

  private def copy(
      start: Option[Message] = start,
      slots: Vector[Slot] = slots,
      usage: JsonObject = usage,
      stopped: Boolean = stopped
  ): MessageAccumulator = new MessageAccumulator(start, slots, usage, stopped)
}

object MessageAccumulator {

  val empty: MessageAccumulator =
    new MessageAccumulator(None, Vector.empty, JsonObject.empty, stopped = false)

  /** Folds a complete, in-memory event sequence. */
  def fold(events: Iterable[StreamEvent]): Either[AccumulatorError, Message] =
    events
      .foldLeft[Either[AccumulatorError, MessageAccumulator]](Right(empty))((acc, e) => acc.flatMap(_.add(e)))
      .flatMap(_.result)

  /**
   * A block under construction. Text arrives in many small deltas, so chunks are kept in a vector and joined
   * once; appending to a string on every delta would be quadratic.
   */
  final private case class Slot(
      block: ContentBlock,
      chunks: Vector[String],
      signature: Option[String],
      citations: Vector[Json]
  ) {
    def materialize(closed: Boolean): ContentBlock = block match {
      case b: ContentBlock.Text =>
        val extras =
          if (citations.isEmpty) b.extras
          else {
            val existing = b.extras("citations").flatMap(_.asArray).getOrElse(Vector.empty)
            b.extras.add("citations", Json.fromValues(existing ++ citations))
          }
        ContentBlock.Text.raw(b.text + chunks.mkString, b.cacheControl, extras)

      case b: ContentBlock.Thinking =>
        ContentBlock.Thinking.raw(b.thinking + chunks.mkString, signature.orElse(b.signature), b.extras)

      case b: ContentBlock.ToolUse if chunks.nonEmpty =>
        val raw = chunks.mkString
        io.circe.parser.parse(raw).toOption.flatMap(_.asObject) match {
          case Some(input)    => ContentBlock.ToolUse.withInput(b, input)
          case None if closed => ContentBlock.ToolUse.invalid(b, raw)
          case None           => b
        }

      case b: ContentBlock.ServerToolUse if chunks.nonEmpty =>
        io.circe.parser.parse(chunks.mkString).toOption.flatMap(_.asObject) match {
          case Some(input) => ContentBlock.ServerToolUse.withInput(b, input)
          case None        => b
        }

      case other => other
    }
  }
}
