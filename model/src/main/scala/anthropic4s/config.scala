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

/**
 * Controls model reasoning.
 *
 * Use [[ThinkingConfig.adaptive]] on current models: the model decides when and how much to think, and
 * [[Effort]] steers the depth. `enabled(budget)` only works on older models (it is a 400 on Opus 4.7+), and
 * several current models reject `disabled` too.
 */
final case class ThinkingConfig private (
    tpe: String,
    budgetTokens: Option[Int],
    display: Option[ThinkingDisplay],
    extras: JsonObject
) {

  /** `Summarized` returns readable reasoning; the default on current models is `Omitted`. */
  def withDisplay(d: ThinkingDisplay): ThinkingConfig = copy(display = Some(d))
}

object ThinkingConfig {
  val adaptive: ThinkingConfig     = new ThinkingConfig("adaptive", None, None, JsonObject.empty)
  val disabled: ThinkingConfig     = new ThinkingConfig("disabled", None, None, JsonObject.empty)
  val betweenTools: ThinkingConfig = new ThinkingConfig("between_tools", None, None, JsonObject.empty)

  def enabled(budgetTokens: Int): ThinkingConfig =
    new ThinkingConfig("enabled", Some(budgetTokens), None, JsonObject.empty)

  @unused private def unapply(t: ThinkingConfig): Some[ThinkingConfig] = Some(t)

  implicit val decoder: Decoder[ThinkingConfig] = Wire.decoder { r =>
    for {
      tpe     <- r.required[String]("type")
      budget  <- r.optional[Int]("budget_tokens")
      display <- r.optional[ThinkingDisplay]("display")
    } yield new ThinkingConfig(tpe, budget, display, r.extras)
  }

  implicit val encoder: Encoder[ThinkingConfig] = Encoder.instance { t =>
    Wire
      .typed(t.tpe)
      .optional("budget_tokens", t.budgetTokens)
      .optional("display", t.display)
      .toJson(t.extras)
  }
}

/** Constrains the response to a JSON Schema (structured outputs). */
final case class OutputFormat private (tpe: String, schema: Option[JsonObject], extras: JsonObject)

object OutputFormat {
  def jsonSchema(schema: JsonObject): OutputFormat =
    new OutputFormat("json_schema", Some(schema), JsonObject.empty)

  @unused private def unapply(f: OutputFormat): Some[OutputFormat] = Some(f)

  implicit val decoder: Decoder[OutputFormat] = Wire.decoder { r =>
    for {
      tpe <- r.required[String]("type"); schema <- r.optional[JsonObject]("schema")
    } yield new OutputFormat(tpe, schema, r.extras)
  }

  implicit val encoder: Encoder[OutputFormat] =
    Encoder.instance(f => Wire.typed(f.tpe).optional("schema", f.schema).toJson(f.extras))
}

/** Output settings: effort level and, for structured outputs, the response format. */
final case class OutputConfig private (
    effort: Option[Effort],
    format: Option[OutputFormat],
    extras: JsonObject
) {
  def withEffort(e: Effort): OutputConfig       = copy(effort = Some(e))
  def withFormat(f: OutputFormat): OutputConfig = copy(format = Some(f))
  def isEmpty: Boolean                          = effort.isEmpty && format.isEmpty && extras.isEmpty
}

object OutputConfig {
  val empty: OutputConfig = new OutputConfig(None, None, JsonObject.empty)

  @unused private def unapply(o: OutputConfig): Some[OutputConfig] = Some(o)

  implicit val decoder: Decoder[OutputConfig] =
    Wire.decoder { r =>
      for {
        e <- r.optional[Effort]("effort"); f <- r.optional[OutputFormat]("format")
      } yield new OutputConfig(e, f, r.extras)
    }

  implicit val encoder: Encoder[OutputConfig] =
    Encoder.instance(o =>
      Wire.writer.optional("effort", o.effort).optional("format", o.format).toJson(o.extras)
    )
}

/**
 * Server-side fallback when a safety classifier declines a request (`stop_reason: "refusal"`). Needs
 * [[Beta.ServerSideFallback]].
 */
sealed trait Fallbacks extends Product with Serializable
object Fallbacks {

  /** Let the API route by refusal category; no model list to maintain. */
  case object Default extends Fallbacks

  /** Try these models in order. */
  final case class Models(models: List[Model]) extends Fallbacks

  implicit val decoder: Decoder[Fallbacks] =
    Decoder[String]
      .emap[Fallbacks](s => if (s == "default") Right(Default) else Left(s"unknown fallbacks mode: $s"))
      .or(Decoder[List[JsonObject]].emap { objs =>
        objs
          .foldRight[Either[String, List[Model]]](Right(Nil)) { (o, acc) =>
            for {
              rest <- acc; m <- o("model").flatMap(_.asString).toRight("fallback entry without model")
            } yield Model.of(m) :: rest
          }
          .map[Fallbacks](Models(_))
      })

  implicit val encoder: Encoder[Fallbacks] = Encoder.instance {
    case Default    => "default".asJson
    case Models(ms) => ms.map(m => JsonObject("model" -> m.asJson)).asJson
  }
}
