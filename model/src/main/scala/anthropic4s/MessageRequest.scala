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
import cats.data.NonEmptyList
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}

import scala.annotation.unused

/**
 * A Messages API request.
 *
 * Build one from the three required parts and refine it with `withX` methods:
 * {{{
 * MessageRequest(Model.ClaudeOpus5_5, maxTokens = 16000, MessageParam.user("Hello"))
 *   .withSystem("You are terse.")
 *   .withThinking(ThinkingConfig.adaptive)
 *   .withEffort(Effort.High)
 * }}}
 *
 * There are no `temperature`, `top_p` or `top_k` fields: the current API version rejects them. Anything this
 * SDK does not model yet can be sent with [[withExtra]].
 */
final case class MessageRequest private (
    model: Model,
    maxTokens: Int,
    messages: NonEmptyList[MessageParam],
    system: Option[SystemPrompt],
    tools: List[ToolDefinition],
    toolChoice: Option[ToolChoice],
    thinking: Option[ThinkingConfig],
    outputConfig: Option[OutputConfig],
    userId: Option[String],
    stopSequences: List[String],
    serviceTier: Option[String],
    cacheControl: Option[CacheControl],
    speed: Option[String],
    inferenceGeo: Option[String],
    fallbacks: Option[Fallbacks],
    extras: JsonObject
) {
  def withModel(m: Model): MessageRequest                          = copy(model = m)
  def withMaxTokens(n: Int): MessageRequest                        = copy(maxTokens = n)
  def withMessages(ms: NonEmptyList[MessageParam]): MessageRequest = copy(messages = ms)
  def addMessages(ms: MessageParam*): MessageRequest               = copy(messages = messages ++ ms.toList)
  def withSystem(text: String): MessageRequest            = copy(system = Some(SystemPrompt.Text(text)))
  def withSystem(prompt: SystemPrompt): MessageRequest    = copy(system = Some(prompt))
  def withTools(ts: List[ToolDefinition]): MessageRequest = copy(tools = ts)
  def addTools(ts: ToolDefinition*): MessageRequest       = copy(tools = tools ++ ts.toList)
  def withToolChoice(tc: ToolChoice): MessageRequest      = copy(toolChoice = Some(tc))
  def withThinking(t: ThinkingConfig): MessageRequest     = copy(thinking = Some(t))
  def withOutputConfig(o: OutputConfig): MessageRequest   = copy(outputConfig = Some(o))
  def withStopSequences(ss: List[String]): MessageRequest = copy(stopSequences = ss)
  def withServiceTier(tier: String): MessageRequest       = copy(serviceTier = Some(tier))
  def withInferenceGeo(geo: String): MessageRequest       = copy(inferenceGeo = Some(geo))
  def withFallbacks(f: Fallbacks): MessageRequest         = copy(fallbacks = Some(f))

  /** Effort lives inside `output_config` on the wire; this sets it without touching the format. */
  def withEffort(e: Effort): MessageRequest =
    copy(outputConfig = Some(outputConfig.getOrElse(OutputConfig.empty).withEffort(e)))

  /** Constrains the reply to a JSON Schema. Prefer `messages.parse[A]`, which also decodes the result. */
  def withOutputFormat(f: OutputFormat): MessageRequest =
    copy(outputConfig = Some(outputConfig.getOrElse(OutputConfig.empty).withFormat(f)))

  /** An opaque id for the end user, used for abuse detection. Never put personal data in it. */
  def withUserId(id: String): MessageRequest = copy(userId = Some(id))

  /** Top-level automatic prompt caching: the API places the breakpoint for you. */
  def withCacheControl(c: CacheControl): MessageRequest = copy(cacheControl = Some(c))

  /** `"fast"` selects fast mode on models that support it. */
  def withSpeed(s: String): MessageRequest = copy(speed = Some(s))

  /** Sends a top-level body field this SDK does not model. A modeled field with the same key wins. */
  def withExtra(key: String, value: Json): MessageRequest = copy(extras = extras.add(key, value))
}

object MessageRequest {

  def apply(model: Model, maxTokens: Int, messages: NonEmptyList[MessageParam]): MessageRequest =
    new MessageRequest(
      model,
      maxTokens,
      messages,
      None,
      Nil,
      None,
      None,
      None,
      None,
      Nil,
      None,
      None,
      None,
      None,
      None,
      JsonObject.empty
    )

  def apply(model: Model, maxTokens: Int, first: MessageParam, rest: MessageParam*): MessageRequest =
    apply(model, maxTokens, NonEmptyList(first, rest.toList))

  @unused private def unapply(r: MessageRequest): Some[MessageRequest] = Some(r)

  implicit val decoder: Decoder[MessageRequest] = Wire.decoder { r =>
    for {
      model     <- r.required[Model]("model")
      maxTokens <- r.required[Int]("max_tokens")
      messages  <- r.required[NonEmptyList[MessageParam]]("messages")
      system    <- r.optional[SystemPrompt]("system")
      tools     <- r.list[ToolDefinition]("tools")
      choice    <- r.optional[ToolChoice]("tool_choice")
      thinking  <- r.optional[ThinkingConfig]("thinking")
      output    <- r.optional[OutputConfig]("output_config")
      metadata  <- r.optional[JsonObject]("metadata")
      stops     <- r.list[String]("stop_sequences")
      tier      <- r.optional[String]("service_tier")
      cc        <- r.optional[CacheControl]("cache_control")
      speed     <- r.optional[String]("speed")
      geo       <- r.optional[String]("inference_geo")
      fallbacks <- r.optional[Fallbacks]("fallbacks")
    } yield {
      val userId     = metadata.flatMap(_("user_id")).flatMap(_.asString)
      val restOfMeta = metadata.map(_.remove("user_id")).filter(_.nonEmpty)
      val extras     = restOfMeta.fold(r.extras)(m => r.extras.add("metadata", Json.fromJsonObject(m)))
      new MessageRequest(
        model,
        maxTokens,
        messages,
        system,
        tools,
        choice,
        thinking,
        output,
        userId,
        stops,
        tier,
        cc,
        speed,
        geo,
        fallbacks,
        extras
      )
    }
  }

  implicit val encoder: Encoder[MessageRequest] = Encoder.instance { m =>
    // `metadata` may also carry unmodeled keys that were preserved in extras on decode.
    val metaExtras = m.extras("metadata").flatMap(_.asObject).getOrElse(JsonObject.empty)
    val metadata   = m.userId.fold(metaExtras)(id => metaExtras.add("user_id", id.asJson))

    Wire.writer
      .field("model", m.model)
      .field("max_tokens", m.maxTokens)
      .field("messages", m.messages)
      .optional("system", m.system)
      .list("tools", m.tools)
      .optional("tool_choice", m.toolChoice)
      .optional("thinking", m.thinking)
      .optional("output_config", m.outputConfig.filterNot(_.isEmpty))
      .optional("metadata", Option(metadata).filter(_.nonEmpty))
      .list("stop_sequences", m.stopSequences)
      .optional("service_tier", m.serviceTier)
      .optional("cache_control", m.cacheControl)
      .optional("speed", m.speed)
      .optional("inference_geo", m.inferenceGeo)
      .optional("fallbacks", m.fallbacks)
      .toJson(m.extras.remove("metadata"))
  }
}

/** A token-counting request: the parts of a [[MessageRequest]] that determine input size. */
final case class CountTokensRequest private (request: MessageRequest)

object CountTokensRequest {
  def from(request: MessageRequest): CountTokensRequest = new CountTokensRequest(request)

  implicit val encoder: Encoder[CountTokensRequest] = Encoder.instance { c =>
    // The endpoint rejects generation-only fields, so they are stripped here.
    val drop =
      List("max_tokens", "stop_sequences", "service_tier", "speed", "inference_geo", "fallbacks", "metadata")
    c.request.asJson.mapObject(obj => drop.foldLeft(obj)(_.remove(_)))
  }
}

/** The result of a token count. */
final case class TokenCount private (inputTokens: Long, extras: JsonObject)

object TokenCount {
  def apply(inputTokens: Long): TokenCount = new TokenCount(inputTokens, JsonObject.empty)

  @unused private def unapply(t: TokenCount): Some[TokenCount] = Some(t)

  implicit val decoder: Decoder[TokenCount] =
    Wire.decoder(r => r.required[Long]("input_tokens").map(new TokenCount(_, r.extras)))
}
