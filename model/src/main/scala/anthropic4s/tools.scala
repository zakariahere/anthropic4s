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

/** A tool the model may call: one you run yourself ([[ToolDefinition.Custom]]) or one the API runs. */
sealed trait ToolDefinition extends Product with Serializable {
  def name: String
}

object ToolDefinition {

  /**
   * A tool you implement. `inputSchema` is a JSON Schema object describing the arguments.
   *
   * With `strict` on, the API guarantees the arguments match the schema; the schema must then set
   * `additionalProperties: false` and list every property as required.
   */
  final case class Custom private[anthropic4s] (
      name: String,
      description: Option[String],
      inputSchema: JsonObject,
      strict: Option[Boolean],
      eagerInputStreaming: Option[Boolean],
      cacheControl: Option[CacheControl],
      extras: JsonObject
  ) extends ToolDefinition {
    def withDescription(d: String): Custom           = copy(description = Some(d))
    def withStrict(on: Boolean): Custom              = copy(strict = Some(on))
    def withEagerInputStreaming(on: Boolean): Custom = copy(eagerInputStreaming = Some(on))
    def withCacheControl(c: CacheControl): Custom    = copy(cacheControl = Some(c))
    def withExtra(key: String, value: Json): Custom  = copy(extras = extras.add(key, value))
  }
  object Custom {
    @unused private def unapply(c: Custom): Some[Custom] = Some(c)
  }

  /**
   * A tool defined by Anthropic, identified by a versioned `type` such as `web_search_20260209`. `options`
   * holds its settings (`max_uses`, `allowed_domains`, ...) exactly as the API names them.
   */
  final case class Server private[anthropic4s] (tpe: String, name: String, options: JsonObject)
      extends ToolDefinition {
    def withOption[A: Encoder](key: String, value: A): Server = copy(options = options.add(key, value.asJson))
  }
  object Server {
    @unused private def unapply(s: Server): Some[Server] = Some(s)
  }

  def custom(name: String, description: String, inputSchema: JsonObject): Custom =
    new Custom(name, Some(description), inputSchema, None, None, None, JsonObject.empty)

  def server(tpe: String, name: String): Server = new Server(tpe, name, JsonObject.empty)

  val webSearch: Server     = server("web_search_20260209", "web_search")
  val webFetch: Server      = server("web_fetch_20260209", "web_fetch")
  val codeExecution: Server = server("code_execution_20260521", "code_execution")
  val bash: Server          = server("bash_20250124", "bash")
  val textEditor: Server    = server("text_editor_20250728", "str_replace_based_edit_tool")
  val memory: Server        = server("memory_20250818", "memory")

  private val customDecoder: Decoder[ToolDefinition] = Wire.decoder { r =>
    r.skip("type")
    for {
      name   <- r.required[String]("name")
      desc   <- r.optional[String]("description")
      schema <- r.required[JsonObject]("input_schema")
      strict <- r.optional[Boolean]("strict")
      eager  <- r.optional[Boolean]("eager_input_streaming")
      cc     <- r.optional[CacheControl]("cache_control")
    } yield new Custom(name, desc, schema, strict, eager, cc, r.extras)
  }

  implicit val decoder: Decoder[ToolDefinition] = Decoder.instance { c =>
    c.get[Option[String]]("type").flatMap {
      case None | Some("custom") => customDecoder(c)
      case Some(tpe)             =>
        for { name <- c.get[String]("name"); obj <- c.as[JsonObject] } yield new Server(
          tpe,
          name,
          obj.remove("type").remove("name")
        )
    }
  }

  implicit val encoder: Encoder[ToolDefinition] = Encoder.instance {
    case t: Custom =>
      Wire.writer
        .field("name", t.name)
        .optional("description", t.description)
        .field("input_schema", t.inputSchema)
        .optional("strict", t.strict)
        .optional("eager_input_streaming", t.eagerInputStreaming)
        .optional("cache_control", t.cacheControl)
        .toJson(t.extras)
    case t: Server => Wire.typed(t.tpe).field("name", t.name).toJson(t.options)
  }
}

/**
 * How the model may use tools.
 *
 * Note: the newest models (Opus 5.5, Sonnet 5.5, Fable 5.1+) reject `any` and `tool` with a 400. Use `auto`
 * plus a strict tool, or structured outputs, instead of forcing a call.
 */
final case class ToolChoice private[anthropic4s] (
    tpe: String,
    name: Option[String],
    disableParallelToolUse: Option[Boolean],
    extras: JsonObject
) {

  /** At most one tool call per turn. */
  def withDisableParallelToolUse(on: Boolean): ToolChoice = copy(disableParallelToolUse = Some(on))
}

object ToolChoice {
  val auto: ToolChoice               = new ToolChoice("auto", None, None, JsonObject.empty)
  val any: ToolChoice                = new ToolChoice("any", None, None, JsonObject.empty)
  val none: ToolChoice               = new ToolChoice("none", None, None, JsonObject.empty)
  def tool(name: String): ToolChoice = new ToolChoice("tool", Some(name), None, JsonObject.empty)

  @unused private def unapply(t: ToolChoice): Some[ToolChoice] = Some(t)

  implicit val decoder: Decoder[ToolChoice] = Wire.decoder { r =>
    for {
      tpe  <- r.required[String]("type")
      name <- r.optional[String]("name")
      dis  <- r.optional[Boolean]("disable_parallel_tool_use")
    } yield new ToolChoice(tpe, name, dis, r.extras)
  }

  implicit val encoder: Encoder[ToolChoice] = Encoder.instance { t =>
    Wire
      .typed(t.tpe)
      .optional("name", t.name)
      .optional("disable_parallel_tool_use", t.disableParallelToolUse)
      .toJson(t.extras)
  }
}
