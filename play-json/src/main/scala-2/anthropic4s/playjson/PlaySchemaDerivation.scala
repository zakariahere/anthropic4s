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
import play.api.libs.json.Format

import scala.reflect.macros.blackbox

private[playjson] trait PlaySchemaDerivation {

  /** The derived schema of `A`, read and written with its Play `Format`. */
  def derived[A](implicit format: Format[A]): Schema[A] = macro PlaySchemaMacros.derived[A]
}

private[playjson] object PlaySchemaMacros {

  // Calling `Schema.derived[A]` explicitly (rather than summoning a `Schema[A]`) matters: an implicit
  // search here would find the very value being defined when this is used to initialise an implicit val.
  def derived[A: c.WeakTypeTag](c: blackbox.Context)(format: c.Tree): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[A]
    q"_root_.anthropic4s.playjson.PlaySchema.from[$tpe](_root_.anthropic4s.schema.Schema.derived[$tpe])($format)"
  }
}
