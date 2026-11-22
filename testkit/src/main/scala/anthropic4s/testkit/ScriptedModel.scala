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

package anthropic4s.testkit

import anthropic4s._
import cats.effect.{Concurrent, Ref}
import cats.syntax.all._
import fs2.Stream
import io.circe.syntax._
import io.circe.{Json, JsonObject}

/** Ready-made replies for [[ScriptedModel]]. */
object Replies {

  private val usage = Usage(10, 5)

  /** The model answers with text and ends its turn. */
  def text(text: String): Message =
    Message("msg_test", Model.ClaudeOpus5_5, List(ContentBlock.Text(text)), StopReason.EndTurn, usage)

  /** The model calls one tool. */
  def toolUse(name: String, input: Json, id: String = "toolu_1"): Message =
    toolUses((id, name, input))

  /** The model calls several tools in one turn; each is `(id, name, input)`. */
  def toolUses(calls: (String, String, Json)*): Message =
    Message(
      "msg_test",
      Model.ClaudeOpus5_5,
      calls.toList.map { case (id, name, input) =>
        ContentBlock.ToolUse(id, name, input.asObject.getOrElse(JsonObject.empty))
      },
      StopReason.ToolUse,
      usage
    )

  /** A reply with the given stop reason and no content, e.g. `StopReason.Refusal`. */
  def stopped(reason: StopReason): Message = Message("msg_test", Model.ClaudeOpus5_5, Nil, reason, usage)

  /** A structured-output reply: the value encoded as the JSON text the API would return. */
  def structured[A](value: A)(implicit schema: anthropic4s.schema.Schema[A]): Message =
    text(schema.encoder(value).noSpaces)
}

/** Turns a complete message into the stream events the API would have sent for it. */
object StreamEvents {

  def of(message: Message): List[StreamEvent] = {
    val blocks = message.content.zipWithIndex.flatMap { case (block, i) =>
      val (empty, deltas): (ContentBlock, List[Delta]) = block match {
        case b: ContentBlock.Text =>
          (
            ContentBlock.Text.raw("", b.cacheControl, b.extras),
            if (b.text.isEmpty) Nil else List(Delta.Text(b.text))
          )
        case b: ContentBlock.ToolUse =>
          (
            ContentBlock.ToolUse(b.id, b.name, JsonObject.empty),
            List(Delta.InputJson(b.input.asJson.noSpaces))
          )
        case b: ContentBlock.Thinking =>
          (
            ContentBlock.Thinking.raw("", None, b.extras),
            (if (b.thinking.isEmpty) Nil else List(Delta.Thinking(b.thinking))) ++ b.signature.map(
              Delta.Signature(_)
            )
          )
        case other => (other, Nil)
      }
      StreamEvent.ContentBlockStart(i, empty) :: deltas.map(StreamEvent.ContentBlockDelta(i, _)) ::: List(
        StreamEvent.ContentBlockStop(i)
      )
    }
    val start = StreamEvent.MessageStart(message.withContent(Nil).withStop(None, None, None))
    val end   = StreamEvent.MessageDelta(
      message.stopReason,
      message.stopSequence,
      message.stopDetails,
      message.usage.asJson.asObject.getOrElse(JsonObject.empty)
    )
    start :: blocks ::: List(end, StreamEvent.MessageStop)
  }
}

/**
 * A stand-in for the model: it answers each request with the next scripted reply and remembers what it was
 * asked. Works for `create`, `stream`, `parse` and the tool runner, with no network involved.
 * {{{
 * for {
 *   model  <- ScriptedModel.of[IO](Replies.toolUse("get_weather", json"""{"city":"Brussels"}"""), Replies.text("Sunny."))
 *   result <- model.toolRunner(List(weatherTool)).run(request)
 *   sent   <- model.requests
 * } yield assertEquals(result.text, "Sunny.")
 * }}}
 */
final class ScriptedModel[F[_]] private (
    script: Ref[F, List[Either[Throwable, Message]]],
    seen: Ref[F, Vector[MessageRequest]]
)(implicit F: Concurrent[F])
    extends Messages[F] {

  /** Every request received so far, in order. */
  def requests: F[Vector[MessageRequest]] = seen.get

  private def next(request: MessageRequest): F[Message] =
    seen.updateAndGet(_ :+ request).flatMap { all =>
      script
        .modify {
          case head :: tail => (tail, Option(head))
          case Nil          => (Nil, None)
        }
        .flatMap {
          case Some(Right(message)) => F.pure(message)
          case Some(Left(error))    => F.raiseError(error)
          case None                 =>
            F.raiseError(
              new IllegalStateException(s"ScriptedModel has no reply left for request #${all.size}")
            )
        }
    }

  def create(request: MessageRequest, options: RequestOptions): F[Message] = next(request)

  def createRaw(request: MessageRequest, options: RequestOptions): F[ApiResponse[Message]] =
    next(request).map(ApiResponse(_, 200, Nil, None))

  def stream(request: MessageRequest, options: RequestOptions): Stream[F, StreamEvent] =
    Stream.eval(next(request)).flatMap(message => Stream.emits(StreamEvents.of(message)))

  def countTokens(request: MessageRequest, options: RequestOptions): F[TokenCount] = F.pure(TokenCount(0L))
}

object ScriptedModel {

  def of[F[_]: Concurrent](replies: Message*): F[ScriptedModel[F]] = scripted(
    replies.map(_.asRight[Throwable]): _*
  )

  /** Like [[of]], but a step can also fail, to test error handling. */
  def scripted[F[_]: Concurrent](steps: Either[Throwable, Message]*): F[ScriptedModel[F]] =
    (
      Ref.of[F, List[Either[Throwable, Message]]](steps.toList),
      Ref.of[F, Vector[MessageRequest]](Vector.empty)
    )
      .mapN(new ScriptedModel[F](_, _))
}
