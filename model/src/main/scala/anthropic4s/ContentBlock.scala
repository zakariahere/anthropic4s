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
import io.circe.{Decoder, Encoder, Json, JsonObject}

import scala.annotation.unused

/** Marks a prompt-cache breakpoint: everything up to and including this block is cached. */
final case class CacheControl private[anthropic4s] (ttl: Option[CacheTtl], extras: JsonObject) {
  def withTtl(ttl: CacheTtl): CacheControl = copy(ttl = Some(ttl))
}
object CacheControl {
  val ephemeral: CacheControl = new CacheControl(None, JsonObject.empty)

  @unused private def unapply(c: CacheControl): Some[CacheControl] = Some(c)

  implicit val decoder: Decoder[CacheControl] = Wire.decoder { r =>
    r.skip("type")
    r.optional[CacheTtl]("ttl").map(new CacheControl(_, r.extras))
  }
  implicit val encoder: Encoder[CacheControl] =
    Encoder.instance(c => Wire.typed("ephemeral").optional("ttl", c.ttl).toJson(c.extras))
}

/** Where the bytes of an image or document come from. */
sealed trait Source extends Product with Serializable
object Source {
  final case class Base64(mediaType: String, data: String) extends Source
  final case class PlainText(data: String)                 extends Source
  final case class Url(url: String)                        extends Source
  final case class File(fileId: String)                    extends Source

  /** A source type this SDK version does not know; it round-trips untouched. */
  final case class Unknown(tpe: String, raw: JsonObject) extends Source

  def pdf(base64Data: String): Source = Base64("application/pdf", base64Data)

  implicit val decoder: Decoder[Source] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "base64" =>
        for { mt <- c.get[String]("media_type"); d <- c.get[String]("data") } yield Base64(mt, d)
      case "text" => c.get[String]("data").map(PlainText(_))
      case "url"  => c.get[String]("url").map(Url(_))
      case "file" => c.get[String]("file_id").map(File(_))
      case other  => c.as[JsonObject].map(Unknown(other, _))
    }
  }

  implicit val encoder: Encoder[Source] = Encoder.instance {
    case Base64(mt, d) => Json.obj("type" -> "base64".asJson, "media_type" -> mt.asJson, "data" -> d.asJson)
    case PlainText(d)  =>
      Json.obj("type" -> "text".asJson, "media_type" -> "text/plain".asJson, "data" -> d.asJson)
    case Url(u)          => Json.obj("type" -> "url".asJson, "url" -> u.asJson)
    case File(id)        => Json.obj("type" -> "file".asJson, "file_id" -> id.asJson)
    case Unknown(_, raw) => Json.fromJsonObject(raw)
  }
}

/**
 * One piece of message content.
 *
 * The same type is used in requests and responses, so an assistant reply can be appended to the next request
 * as-is. Match by type and keep a default branch: new block types arrive as [[ContentBlock.Unknown]] and are
 * sent back unchanged, which is what keeps server-tool results and thinking blocks intact across turns.
 */
sealed trait ContentBlock extends Product with Serializable {

  /** Fields this SDK version does not model. They are written back on encode. */
  def extras: JsonObject
}

object ContentBlock {

  final case class Text private[anthropic4s] (
      text: String,
      cacheControl: Option[CacheControl],
      extras: JsonObject
  ) extends ContentBlock {
    def withCacheControl(c: CacheControl): Text   = copy(cacheControl = Some(c))
    def withExtra(key: String, value: Json): Text = copy(extras = extras.add(key, value))
  }
  object Text {
    def apply(text: String): Text      = new Text(text, None, JsonObject.empty)
    def unapply(t: Text): Some[String] = Some(t.text)
    private[anthropic4s] def raw(text: String, cc: Option[CacheControl], extras: JsonObject): Text =
      new Text(text, cc, extras)
  }

  final case class Image private[anthropic4s] (
      source: Source,
      cacheControl: Option[CacheControl],
      extras: JsonObject
  ) extends ContentBlock {
    def withCacheControl(c: CacheControl): Image = copy(cacheControl = Some(c))
  }
  object Image {
    def apply(source: Source): Image    = new Image(source, None, JsonObject.empty)
    def unapply(i: Image): Some[Source] = Some(i.source)
  }

  final case class Document private[anthropic4s] (
      source: Source,
      title: Option[String],
      context: Option[String],
      cacheControl: Option[CacheControl],
      extras: JsonObject
  ) extends ContentBlock {
    def withTitle(t: String): Document              = copy(title = Some(t))
    def withContext(c: String): Document            = copy(context = Some(c))
    def withCacheControl(c: CacheControl): Document = copy(cacheControl = Some(c))

    /** Asks the model to cite passages of this document. Enable it on all documents of a request or none. */
    def withCitations(enabled: Boolean): Document =
      copy(extras = extras.add("citations", Json.obj("enabled" -> enabled.asJson)))
  }
  object Document {
    def apply(source: Source): Document = new Document(source, None, None, None, JsonObject.empty)
    @unused private def unapply(d: Document): Some[Document] = Some(d)
  }

  /** The model asks you to run one of your tools. `input` is the parsed arguments object. */
  final case class ToolUse private[anthropic4s] (
      id: String,
      name: String,
      input: JsonObject,
      invalidInputJson: Option[String],
      extras: JsonObject
  ) extends ContentBlock
  object ToolUse {
    def apply(id: String, name: String, input: JsonObject): ToolUse =
      new ToolUse(id, name, input, None, JsonObject.empty)
    def unapply(t: ToolUse): Some[(String, String, JsonObject)] = Some((t.id, t.name, t.input))

    /**
     * A streamed tool call whose accumulated arguments were not valid JSON (for example because the response
     * hit `max_tokens`). `invalidInputJson` is local only; it is never sent to the API.
     */
    private[anthropic4s] def invalid(t: ToolUse, rawJson: String): ToolUse =
      new ToolUse(t.id, t.name, JsonObject.empty, Some(rawJson), t.extras)
    private[anthropic4s] def withInput(t: ToolUse, input: JsonObject): ToolUse =
      new ToolUse(t.id, t.name, input, None, t.extras)
  }

  /** Your answer to a [[ToolUse]]. All results of one turn go into a single user message. */
  final case class ToolResult private[anthropic4s] (
      toolUseId: String,
      content: Option[ToolResultContent],
      isError: Boolean,
      cacheControl: Option[CacheControl],
      extras: JsonObject
  ) extends ContentBlock {
    def withCacheControl(c: CacheControl): ToolResult = copy(cacheControl = Some(c))

    /** The textual part of the result; images and other blocks are left out. */
    def text: String = content match {
      case Some(ToolResultContent.Text(value))    => value
      case Some(ToolResultContent.Blocks(blocks)) => blocks.collect { case Text(t) => t }.mkString
      case None                                   => ""
    }
  }
  object ToolResult {
    def apply(toolUseId: String, content: String): ToolResult =
      new ToolResult(toolUseId, Some(ToolResultContent.Text(content)), false, None, JsonObject.empty)
    def apply(toolUseId: String, content: ToolResultContent): ToolResult =
      new ToolResult(toolUseId, Some(content), false, None, JsonObject.empty)
    def error(toolUseId: String, message: String): ToolResult =
      new ToolResult(toolUseId, Some(ToolResultContent.Text(message)), true, None, JsonObject.empty)
    @unused private def unapply(t: ToolResult): Some[ToolResult] = Some(t)
  }

  /** Model reasoning. Send these blocks back unchanged; the signature ties them to the conversation. */
  final case class Thinking private[anthropic4s] (
      thinking: String,
      signature: Option[String],
      extras: JsonObject
  ) extends ContentBlock
  object Thinking {
    def unapply(t: Thinking): Some[String] = Some(t.thinking)
    private[anthropic4s] def raw(thinking: String, signature: Option[String], extras: JsonObject): Thinking =
      new Thinking(thinking, signature, extras)
  }

  final case class RedactedThinking private[anthropic4s] (data: String, extras: JsonObject)
      extends ContentBlock
  object RedactedThinking {
    @unused private def unapply(t: RedactedThinking): Some[RedactedThinking] = Some(t)
  }

  /** A tool the API runs on its side (web search, code execution, ...). Nothing for you to execute. */
  final case class ServerToolUse private[anthropic4s] (
      id: String,
      name: String,
      input: JsonObject,
      extras: JsonObject
  ) extends ContentBlock
  object ServerToolUse {
    def unapply(t: ServerToolUse): Some[(String, String, JsonObject)] = Some((t.id, t.name, t.input))
    private[anthropic4s] def withInput(t: ServerToolUse, input: JsonObject): ServerToolUse =
      new ServerToolUse(t.id, t.name, input, t.extras)
  }

  /** A block type this SDK version does not model. `extras` holds the whole object, `type` included. */
  final case class Unknown(tpe: String, extras: JsonObject) extends ContentBlock

  // -- codecs ----------------------------------------------------------------------------------------

  private val textDecoder: Decoder[ContentBlock] = Wire.decoder { r =>
    r.skip("type")
    for { t <- r.required[String]("text"); cc <- r.optional[CacheControl]("cache_control") } yield new Text(
      t,
      cc,
      r.extras
    )
  }

  private val imageDecoder: Decoder[ContentBlock] = Wire.decoder { r =>
    r.skip("type")
    for {
      s <- r.required[Source]("source"); cc <- r.optional[CacheControl]("cache_control")
    } yield new Image(s, cc, r.extras)
  }

  private val documentDecoder: Decoder[ContentBlock] = Wire.decoder { r =>
    r.skip("type")
    for {
      s   <- r.required[Source]("source")
      t   <- r.optional[String]("title")
      ctx <- r.optional[String]("context")
      cc  <- r.optional[CacheControl]("cache_control")
    } yield new Document(s, t, ctx, cc, r.extras)
  }

  private val toolUseDecoder: Decoder[ContentBlock] = Wire.decoder { r =>
    r.skip("type")
    for {
      id    <- r.required[String]("id")
      name  <- r.required[String]("name")
      input <- r.optional[JsonObject]("input")
    } yield new ToolUse(id, name, input.getOrElse(JsonObject.empty), None, r.extras)
  }

  private val serverToolUseDecoder: Decoder[ContentBlock] = Wire.decoder { r =>
    r.skip("type")
    for {
      id    <- r.required[String]("id")
      name  <- r.required[String]("name")
      input <- r.optional[JsonObject]("input")
    } yield new ServerToolUse(id, name, input.getOrElse(JsonObject.empty), r.extras)
  }

  private lazy val toolResultDecoder: Decoder[ContentBlock] = Wire.decoder { r =>
    r.skip("type")
    for {
      id  <- r.required[String]("tool_use_id")
      c   <- r.optional[ToolResultContent]("content")
      err <- r.optional[Boolean]("is_error")
      cc  <- r.optional[CacheControl]("cache_control")
    } yield new ToolResult(id, c, err.getOrElse(false), cc, r.extras)
  }

  private val thinkingDecoder: Decoder[ContentBlock] = Wire.decoder { r =>
    r.skip("type")
    for { t <- r.optional[String]("thinking"); s <- r.optional[String]("signature") } yield new Thinking(
      t.getOrElse(""),
      s,
      r.extras
    )
  }

  private val redactedThinkingDecoder: Decoder[ContentBlock] = Wire.decoder { r =>
    r.skip("type")
    r.required[String]("data").map(new RedactedThinking(_, r.extras))
  }

  implicit lazy val decoder: Decoder[ContentBlock] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "text"              => textDecoder(c)
      case "image"             => imageDecoder(c)
      case "document"          => documentDecoder(c)
      case "tool_use"          => toolUseDecoder(c)
      case "tool_result"       => toolResultDecoder(c)
      case "thinking"          => thinkingDecoder(c)
      case "redacted_thinking" => redactedThinkingDecoder(c)
      case "server_tool_use"   => serverToolUseDecoder(c)
      case other               => c.as[JsonObject].map(Unknown(other, _))
    }
  }

  implicit lazy val encoder: Encoder[ContentBlock] = Encoder.instance {
    case b: Text =>
      Wire.typed("text").field("text", b.text).optional("cache_control", b.cacheControl).toJson(b.extras)
    case b: Image =>
      Wire.typed("image").field("source", b.source).optional("cache_control", b.cacheControl).toJson(b.extras)
    case b: Document =>
      Wire
        .typed("document")
        .field("source", b.source)
        .optional("title", b.title)
        .optional("context", b.context)
        .optional("cache_control", b.cacheControl)
        .toJson(b.extras)
    case b: ToolUse =>
      Wire.typed("tool_use").field("id", b.id).field("name", b.name).field("input", b.input).toJson(b.extras)
    case b: ServerToolUse =>
      Wire
        .typed("server_tool_use")
        .field("id", b.id)
        .field("name", b.name)
        .field("input", b.input)
        .toJson(b.extras)
    case b: ToolResult =>
      val base = Wire.typed("tool_result").field("tool_use_id", b.toolUseId).optional("content", b.content)
      (if (b.isError) base.field("is_error", true) else base)
        .optional("cache_control", b.cacheControl)
        .toJson(b.extras)
    case b: Thinking =>
      Wire.typed("thinking").field("thinking", b.thinking).optional("signature", b.signature).toJson(b.extras)
    case b: RedactedThinking =>
      Wire.typed("redacted_thinking").field("data", b.data).toJson(b.extras)
    case b: Unknown => Json.fromJsonObject(b.extras)
  }
}

/** What a tool returned: plain text, or richer blocks such as images. */
sealed trait ToolResultContent extends Product with Serializable
object ToolResultContent {
  final case class Text(value: String)                extends ToolResultContent
  final case class Blocks(blocks: List[ContentBlock]) extends ToolResultContent

  implicit lazy val decoder: Decoder[ToolResultContent] =
    Decoder[String].map[ToolResultContent](Text(_)).or(Decoder[List[ContentBlock]].map(Blocks(_)))

  implicit lazy val encoder: Encoder[ToolResultContent] = Encoder.instance {
    case Text(v)    => v.asJson
    case Blocks(bs) => bs.asJson
  }
}
