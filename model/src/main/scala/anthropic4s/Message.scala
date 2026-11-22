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

import anthropic4s.internal.Wire
import io.circe.{Decoder, Encoder, JsonObject}

import scala.annotation.unused

/** Extra detail the API attaches when `stop_reason` is `refusal`. */
final case class StopDetails private (
    tpe: Option[String],
    category: Option[String],
    explanation: Option[String],
    extras: JsonObject
)

object StopDetails {
  @unused private def unapply(s: StopDetails): Some[StopDetails] = Some(s)

  implicit val decoder: Decoder[StopDetails] = Wire.decoder { r =>
    for {
      tpe <- r.optional[String]("type")
      cat <- r.optional[String]("category")
      exp <- r.optional[String]("explanation")
    } yield new StopDetails(tpe, cat, exp, r.extras)
  }

  implicit val encoder: Encoder[StopDetails] = Encoder.instance { s =>
    Wire.writer
      .optional("type", s.tpe)
      .optional("category", s.category)
      .optional("explanation", s.explanation)
      .toJson(s.extras)
  }
}

/**
 * The model's reply.
 *
 * Always check [[stopReason]] before trusting [[content]]: a `Refusal` or `MaxTokens` stop means the content
 * is absent or cut short.
 */
final case class Message private (
    id: String,
    model: Model,
    content: List[ContentBlock],
    stopReason: Option[StopReason],
    stopSequence: Option[String],
    stopDetails: Option[StopDetails],
    usage: Usage,
    extras: JsonObject
) {

  /** All text blocks joined together. Thinking and tool blocks are not included. */
  def text: String = content.collect { case ContentBlock.Text(t) => t }.mkString

  /** The tool calls you are expected to execute. */
  def toolUses: List[ContentBlock.ToolUse] = content.collect { case t: ContentBlock.ToolUse => t }

  /**
   * This reply as an assistant turn for the next request. It carries the complete content (thinking,
   * server-tool and unknown blocks included), which the API needs to continue the conversation.
   */
  def toParam: MessageParam = MessageParam.assistant(content)

  def isRefusal: Boolean = stopReason.contains(StopReason.Refusal)

  private[anthropic4s] def withContent(c: List[ContentBlock]): Message = copy(content = c)
  private[anthropic4s] def withStop(
      reason: Option[StopReason],
      sequence: Option[String],
      details: Option[StopDetails]
  ): Message = copy(stopReason = reason, stopSequence = sequence, stopDetails = details)
  private[anthropic4s] def withUsage(u: Usage): Message = copy(usage = u)
}

object Message {

  /** Builds a reply by hand. Meant for tests and fakes; real ones come from the API. */
  def apply(
      id: String,
      model: Model,
      content: List[ContentBlock],
      stopReason: StopReason,
      usage: Usage
  ): Message =
    new Message(id, model, content, Some(stopReason), None, None, usage, JsonObject.empty)

  @unused private def unapply(m: Message): Some[Message] = Some(m)

  implicit val decoder: Decoder[Message] = Wire.decoder { r =>
    r.skip("type")
    r.skip("role")
    for {
      id      <- r.required[String]("id")
      model   <- r.required[Model]("model")
      content <- r.list[ContentBlock]("content")
      reason  <- r.optional[StopReason]("stop_reason")
      seq     <- r.optional[String]("stop_sequence")
      details <- r.optional[StopDetails]("stop_details")
      usage   <- r.optional[Usage]("usage")
    } yield new Message(id, model, content, reason, seq, details, usage.getOrElse(Usage.empty), r.extras)
  }

  implicit val encoder: Encoder[Message] = Encoder.instance { m =>
    Wire
      .typed("message")
      .field("id", m.id)
      .field("role", Role.Assistant)
      .field("model", m.model)
      .field("content", m.content)
      .optional("stop_reason", m.stopReason)
      .optional("stop_sequence", m.stopSequence)
      .optional("stop_details", m.stopDetails)
      .field("usage", m.usage)
      .toJson(m.extras)
  }
}

/** The `error` object of an API error response or a mid-stream `error` event. */
final case class ApiErrorBody private (
    errorType: String,
    message: String,
    requestId: Option[String],
    raw: JsonObject
)

object ApiErrorBody {
  def apply(errorType: String, message: String): ApiErrorBody =
    new ApiErrorBody(errorType, message, None, JsonObject.empty)

  @unused private def unapply(e: ApiErrorBody): Some[ApiErrorBody] = Some(e)

  implicit val decoder: Decoder[ApiErrorBody] = Decoder.instance { c =>
    for {
      raw <- c.as[JsonObject]
      // Either the full envelope `{"type":"error","error":{...},"request_id":...}` or the inner object.
      err = c.downField("error").success.getOrElse(c)
      tpe <- err.get[Option[String]]("type")
      msg <- err.get[Option[String]]("message")
      rid <- c.get[Option[String]]("request_id")
    } yield new ApiErrorBody(tpe.getOrElse("unknown_error"), msg.getOrElse(""), rid, raw)
  }
}
