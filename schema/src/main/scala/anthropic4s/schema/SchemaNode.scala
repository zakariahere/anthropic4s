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

package anthropic4s.schema

import io.circe.syntax._
import io.circe.{Json, JsonObject}

import scala.annotation.StaticAnnotation

/** Describes a field or type to the model. Good descriptions noticeably improve results. */
final class description(val text: String) extends StaticAnnotation

/** Overrides the JSON name of a field, or the tag of a sealed-trait case. */
final class name(val value: String) extends StaticAnnotation

/** Overrides the tag field of a sealed trait (default: `type`). */
final class discriminator(val field: String) extends StaticAnnotation

/**
 * The shape of a type, in the subset of JSON Schema that the API's strict mode can enforce: every object
 * property is required, objects are closed, and an optional value is "required but nullable".
 */
sealed trait SchemaNode extends Product with Serializable
object SchemaNode {
  final case class Str(format: Option[String]) extends SchemaNode
  case object Integer                          extends SchemaNode
  case object Number                           extends SchemaNode
  case object Bool                             extends SchemaNode
  final case class Arr(items: SchemaNode)      extends SchemaNode

  /** A closed set of strings; what a sealed trait of case objects becomes. */
  final case class Enum(values: List[String]) extends SchemaNode

  final case class Field(name: String, schema: SchemaNode, description: Option[String])
  final case class Obj(fields: List[Field], description: Option[String]) extends SchemaNode

  /** A case object used on its own: an object without properties. */
  final case class Singleton(tag: String) extends SchemaNode

  /** A sealed trait: one object shape per case, told apart by a constant tag field. */
  final case class Variant(tag: String, fields: List[Field], description: Option[String])
  final case class OneOf(discriminator: String, variants: List[Variant], description: Option[String])
      extends SchemaNode

  /** `Option[A]`: the value or `null`. The property itself stays required. */
  final case class Nullable(inner: SchemaNode) extends SchemaNode

  /** A hand-written schema, passed through as-is and not checked. */
  final case class Raw(json: JsonObject) extends SchemaNode

  private def described(obj: JsonObject, description: Option[String]): JsonObject =
    description.fold(obj)(d => obj.add("description", d.asJson))

  private def objectJson(
      fields: List[Field],
      tag: Option[(String, String)],
      description: Option[String]
  ): JsonObject = {
    val tagProperty = tag.map { case (field, value) =>
      field -> Json.obj("type" -> "string".asJson, "const" -> value.asJson)
    }
    val properties = tagProperty.toList ++ fields.map(f =>
      f.name -> Json.fromJsonObject(described(toJson(f.schema), f.description))
    )
    described(
      JsonObject(
        "type"                 -> "object".asJson,
        "properties"           -> Json.fromFields(properties),
        "required"             -> properties.map(_._1).asJson,
        "additionalProperties" -> false.asJson
      ),
      description
    )
  }

  def toJson(node: SchemaNode): JsonObject = node match {
    case Str(format) =>
      format.foldLeft(JsonObject("type" -> "string".asJson))((o, f) => o.add("format", f.asJson))
    case Integer        => JsonObject("type" -> "integer".asJson)
    case Number         => JsonObject("type" -> "number".asJson)
    case Bool           => JsonObject("type" -> "boolean".asJson)
    case Arr(items)     => JsonObject("type" -> "array".asJson, "items" -> Json.fromJsonObject(toJson(items)))
    case Enum(values)   => JsonObject("type" -> "string".asJson, "enum" -> values.asJson)
    case Obj(fields, d) => objectJson(fields, None, d)
    case Singleton(_)   => objectJson(Nil, None, None)
    case OneOf(disc, variants, d) =>
      described(
        JsonObject(
          "anyOf" -> variants
            .map(v => Json.fromJsonObject(objectJson(v.fields, Some(disc -> v.tag), v.description)))
            .asJson
        ),
        d
      )
    case Nullable(inner) =>
      JsonObject(
        "anyOf" -> List(Json.fromJsonObject(toJson(inner)), Json.obj("type" -> "null".asJson)).asJson
      )
    case Raw(json) => json
  }
}

/** Why a derived schema cannot be used in strict mode. */
sealed abstract class SchemaIssue(val message: String) extends Product with Serializable
object SchemaIssue {
  case object RootNotObject
      extends SchemaIssue("the top-level type must be a case class (an object); wrap other types in one")
  final case class DiscriminatorCollision(variant: String, field: String)
      extends SchemaIssue(
        s"case '$variant' has a field named '$field', which is also the tag field; rename one with @name or @discriminator"
      )
  final case class DuplicateTag(tag: String)
      extends SchemaIssue(s"two cases of a sealed trait share the tag '$tag'; give one a different @name")
  final case class EmptyEnum(at: String) extends SchemaIssue(s"the enum at '$at' has no values")
}
