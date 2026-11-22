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
import io.circe.syntax._
import io.circe.{Decoder, Encoder, JsonObject}

import scala.annotation.unused

/** The body of a message: a plain string, or a list of content blocks. */
sealed trait MessageContent extends Product with Serializable {
  def blocks: List[ContentBlock] = this match {
    case MessageContent.Text(value)    => List(ContentBlock.Text(value))
    case MessageContent.Blocks(blocks) => blocks
  }
}
object MessageContent {
  final case class Text(value: String)                             extends MessageContent
  final case class Blocks(override val blocks: List[ContentBlock]) extends MessageContent

  implicit val decoder: Decoder[MessageContent] =
    Decoder[String].map[MessageContent](Text(_)).or(Decoder[List[ContentBlock]].map(Blocks(_)))

  implicit val encoder: Encoder[MessageContent] = Encoder.instance {
    case Text(v)    => v.asJson
    case Blocks(bs) => bs.asJson
  }
}

/** One turn of the conversation you send to the API. */
final case class MessageParam private (role: Role, content: MessageContent, extras: JsonObject)

object MessageParam {

  def apply(role: Role, content: MessageContent): MessageParam =
    new MessageParam(role, content, JsonObject.empty)

  def user(text: String): MessageParam = apply(Role.User, MessageContent.Text(text))
  def user(first: ContentBlock, rest: ContentBlock*): MessageParam =
    apply(Role.User, MessageContent.Blocks(first :: rest.toList))
  def user(blocks: List[ContentBlock]): MessageParam = apply(Role.User, MessageContent.Blocks(blocks))

  def assistant(text: String): MessageParam               = apply(Role.Assistant, MessageContent.Text(text))
  def assistant(blocks: List[ContentBlock]): MessageParam =
    apply(Role.Assistant, MessageContent.Blocks(blocks))

  /**
   * An operator instruction placed mid-conversation. Unlike editing the top-level system prompt, this keeps
   * the cached prefix valid. It must follow a user message.
   */
  def system(text: String): MessageParam = apply(Role.System, MessageContent.Text(text))

  @unused private def unapply(m: MessageParam): Some[MessageParam] = Some(m)

  implicit val decoder: Decoder[MessageParam] = Wire.decoder { r =>
    for {
      role <- r.required[Role]("role"); content <- r.required[MessageContent]("content")
    } yield new MessageParam(role, content, r.extras)
  }

  implicit val encoder: Encoder[MessageParam] =
    Encoder.instance(m => Wire.writer.field("role", m.role).field("content", m.content).toJson(m.extras))
}

/** The top-level system prompt: a string, or text blocks when you need cache breakpoints. */
sealed trait SystemPrompt extends Product with Serializable
object SystemPrompt {
  final case class Text(value: String)                extends SystemPrompt
  final case class Blocks(blocks: List[ContentBlock]) extends SystemPrompt

  /** A single-block system prompt with a cache breakpoint at its end. */
  def cached(text: String): SystemPrompt =
    Blocks(List(ContentBlock.Text(text).withCacheControl(CacheControl.ephemeral)))

  implicit val decoder: Decoder[SystemPrompt] =
    Decoder[String].map[SystemPrompt](Text(_)).or(Decoder[List[ContentBlock]].map(Blocks(_)))

  implicit val encoder: Encoder[SystemPrompt] = Encoder.instance {
    case Text(v)    => v.asJson
    case Blocks(bs) => bs.asJson
  }
}
