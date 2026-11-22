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

import io.circe.{Json, JsonObject}

/**
 * Checks a JSON value against the subset of JSON Schema this library produces (`type`, `properties`,
 * `required`, `additionalProperties: false`, `items`, `enum`, `const`, `anyOf`).
 *
 * It exists so tests can prove that what an encoder writes is what its schema promises. It is not a
 * general-purpose JSON Schema validator: keywords outside that subset are ignored.
 */
object SchemaValidator {

  /** The problems found, each prefixed with the path of the offending value. Empty means valid. */
  def validate(schema: JsonObject, value: Json): List[String] = check(schema, value, "$")

  private def check(schema: JsonObject, value: Json, path: String): List[String] = {
    val anyOf = schema("anyOf").flatMap(_.asArray).map(_.flatMap(_.asObject))
    anyOf match {
      case Some(options) =>
        if (options.exists(check(_, value, path).isEmpty)) Nil
        else List(s"$path: matches none of the ${options.size} allowed shapes")
      case None =>
        val const =
          schema("const").filter(_ != value).map(c => s"$path: expected the constant ${c.noSpaces}").toList
        val enumIssue = schema("enum")
          .flatMap(_.asArray)
          .filterNot(_.contains(value))
          .map(vs => s"$path: ${value.noSpaces} is not one of ${vs.map(_.noSpaces).mkString(", ")}")
          .toList
        const ++ enumIssue ++ schema("type")
          .flatMap(_.asString)
          .toList
          .flatMap(checkType(_, schema, value, path))
    }
  }

  private def checkType(tpe: String, schema: JsonObject, value: Json, path: String): List[String] =
    tpe match {
      case "object" =>
        value.asObject match {
          case None      => List(s"$path: expected an object")
          case Some(obj) =>
            val properties = schema("properties").flatMap(_.asObject).getOrElse(JsonObject.empty)
            val required   = schema("required").flatMap(_.asArray).getOrElse(Vector.empty).flatMap(_.asString)
            val closed     = schema("additionalProperties").flatMap(_.asBoolean).contains(false)
            val missing    =
              required.filterNot(obj.contains).map(k => s"$path: missing required property '$k'").toList
            val extra =
              if (closed)
                obj.keys.filterNot(properties.contains).map(k => s"$path: unexpected property '$k'").toList
              else Nil
            val nested = properties.toList.flatMap { case (k, sub) =>
              (obj(k), sub.asObject) match {
                case (Some(v), Some(s)) => check(s, v, s"$path.$k")
                case _                  => Nil
              }
            }
            missing ++ extra ++ nested
        }
      case "array" =>
        value.asArray match {
          case None        => List(s"$path: expected an array")
          case Some(items) =>
            schema("items").flatMap(_.asObject).toList.flatMap { s =>
              items.zipWithIndex.flatMap { case (v, i) => check(s, v, s"$path[$i]") }
            }
        }
      case "string"  => if (value.isString) Nil else List(s"$path: expected a string")
      case "boolean" => if (value.isBoolean) Nil else List(s"$path: expected a boolean")
      case "null"    => if (value.isNull) Nil else List(s"$path: expected null")
      case "number"  => if (value.isNumber) Nil else List(s"$path: expected a number")
      case "integer" =>
        if (value.asNumber.exists(n => n.toBigInt.isDefined)) Nil else List(s"$path: expected an integer")
      case _ => Nil
    }
}
