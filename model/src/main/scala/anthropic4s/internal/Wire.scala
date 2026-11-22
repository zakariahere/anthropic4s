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

package anthropic4s.internal

import io.circe.{Decoder, Encoder, HCursor, Json, JsonObject}

import scala.collection.mutable

/**
 * Reads the fields of one JSON object and remembers which keys were consumed.
 *
 * Whatever is left over becomes the value's `extras`, so a field the API adds tomorrow survives a
 * decode/encode round trip even though this SDK has never heard of it.
 */
final private[anthropic4s] class ObjectReader private[internal] (cursor: HCursor, obj: JsonObject) {
  private val consumed = mutable.Set.empty[String]

  /** A field that must be present. */
  def required[A: Decoder](key: String): Decoder.Result[A] = {
    consumed += key
    cursor.get[A](key)
  }

  /** A field that may be absent or `null`; both decode to `None`. */
  def optional[A: Decoder](key: String): Decoder.Result[Option[A]] = {
    consumed += key
    cursor.get[Option[A]](key)
  }

  /** A list field where absent, `null` and `[]` all mean "nothing". */
  def list[A: Decoder](key: String): Decoder.Result[List[A]] =
    optional[List[A]](key).map(_.getOrElse(Nil))

  /** Marks a key as handled without reading it (used for the `type` discriminator). */
  def skip(key: String): Unit = { consumed += key; () }

  /** Every key no call above asked for. */
  def extras: JsonObject = obj.filterKeys(k => !consumed.contains(k))
}

/** Builds one JSON object field by field. `None` and empty lists are left out entirely. */
final private[anthropic4s] class ObjectWriter private[internal] (fields: Vector[(String, Json)]) {

  def field[A](key: String, value: A)(implicit enc: Encoder[A]): ObjectWriter =
    new ObjectWriter(fields :+ (key -> enc(value)))

  def optional[A](key: String, value: Option[A])(implicit enc: Encoder[A]): ObjectWriter =
    value.fold(this)(field(key, _))

  def list[A](key: String, values: List[A])(implicit enc: Encoder[A]): ObjectWriter =
    if (values.isEmpty) this else field(key, values)

  /** Known fields win over an `extras` entry with the same key. */
  def toJson(extras: JsonObject): Json =
    Json.fromJsonObject(fields.foldLeft(extras) { case (acc, (k, v)) => acc.add(k, v) })
}

private[anthropic4s] object Wire {

  def decoder[A](read: ObjectReader => Decoder.Result[A]): Decoder[A] =
    Decoder.instance { cursor =>
      cursor.as[JsonObject].flatMap(obj => read(new ObjectReader(cursor, obj)))
    }

  val writer: ObjectWriter = new ObjectWriter(Vector.empty)

  /** Starts an object whose first field is the `type` discriminator. */
  def typed(tpe: String): ObjectWriter = writer.field("type", tpe)
}
