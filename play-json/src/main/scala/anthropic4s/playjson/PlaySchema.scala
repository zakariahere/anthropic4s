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

package anthropic4s.playjson

import anthropic4s.schema.Schema
import io.circe.{Decoder, DecodingFailure, Encoder, Json => CirceJson}
import play.api.libs.json._

/**
 * Structured outputs and tool inputs driven by Play JSON.
 *
 * The JSON Schema still comes from the shape of your case class; reading and writing go through your
 * `Format`. Declare the schema next to the format:
 * {{{
 * object Book {
 *   implicit val config: JsonConfiguration = PlaySchema.configuration
 *   implicit val format: OFormat[Book]     = Json.format[Book]
 *   implicit val schema: Schema[Book]      = PlaySchema.derived[Book]
 * }
 *
 * client.messages.parse[Book](request)
 * }}}
 * Only the outermost type needs a `Schema`; its `Format` already knows how to read the nested ones.
 *
 * The schema describes one JSON shape and your `Format` must read that shape. For case classes written with
 * `Json.format` that holds as long as [[configuration]] is in implicit scope when the macro runs: sealed
 * traits are then tagged with a `type` field holding the short class name, and a missing `Option` is written
 * as `null`. A sealed trait of case objects is described as a string enum, so give it a `Format` that reads
 * and writes plain strings; [[enumFormat]] builds one.
 *
 * Note that `Json.format` cannot see the cases of a sealed trait when they are declared inside the same
 * companion object that calls it. Declare the cases at the top level, or the format elsewhere.
 */
object PlaySchema extends PlaySchemaDerivation {

  /**
   * The `Json.format` settings that match the derived schema. Declare it implicitly before calling the macro:
   * `implicit val config: JsonConfiguration = PlaySchema.configuration`.
   */
  val configuration: JsonConfiguration.Aux[Json.MacroOptions] =
    JsonConfiguration(
      discriminator = "type",
      // Play hands the fully qualified class name to this function; the schema uses the short one.
      typeNaming = JsonNaming(fullName => fullName.split('.').last),
      optionHandlers = OptionHandlers.WritesNull
    )

  /** Keeps the schema of `structure` and swaps its codec for a Play `Format`. */
  def from[A](structure: Schema[A])(implicit format: Format[A]): Schema[A] =
    structure.withCodec(decoder(format), encoder(format))

  /** A `Format` for a sealed trait of case objects: each value is read and written as its name. */
  def enumFormat[A](values: (String, A)*): Format[A] = {
    val byName = values.toMap
    val names  = values.map(_.swap).toMap
    Format(
      Reads {
        case JsString(s) =>
          byName
            .get(s)
            .fold[JsResult[A]](JsError(s"'$s' is not one of ${byName.keys.mkString(", ")}"))(JsSuccess(_))
        case _ => JsError("expected a string")
      },
      Writes(a => JsString(names(a)))
    )
  }

  def decoder[A](implicit reads: Reads[A]): Decoder[A] = Decoder.instance { c =>
    reads.reads(toPlay(c.value)) match {
      case JsSuccess(value, _) => Right(value)
      case JsError(errors)     =>
        val detail =
          errors.map { case (path, es) => s"$path: ${es.flatMap(_.messages).mkString(", ")}" }.mkString("; ")
        Left(DecodingFailure(detail, c.history))
    }
  }

  def encoder[A](implicit writes: Writes[A]): Encoder[A] = Encoder.instance(a => toCirce(writes.writes(a)))

  def toCirce(value: JsValue): CirceJson = value match {
    case JsNull           => CirceJson.Null
    case b: JsBoolean     => CirceJson.fromBoolean(b.value)
    case JsNumber(n)      => CirceJson.fromBigDecimal(n)
    case JsString(s)      => CirceJson.fromString(s)
    case JsArray(items)   => CirceJson.fromValues(items.map(toCirce))
    case JsObject(fields) => CirceJson.fromFields(fields.map { case (k, v) => k -> toCirce(v) })
  }

  def toPlay(value: CirceJson): JsValue =
    value.fold[JsValue](
      JsNull,
      JsBoolean(_),
      n => JsNumber(n.toBigDecimal.getOrElse(BigDecimal(n.toDouble))),
      JsString(_),
      items => JsArray(items.map(toPlay)),
      obj => JsObject(obj.toList.map { case (k, v) => k -> toPlay(v) })
    )
}
