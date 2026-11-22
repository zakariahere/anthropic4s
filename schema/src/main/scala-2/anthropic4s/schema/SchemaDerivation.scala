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

import magnolia1._

/** Scala 2 glue: translates Magnolia's view of a type into [[Derive]] calls. All logic lives there. */
private[schema] trait SchemaDerivation {

  type Typeclass[A] = Schema[A]

  private def descriptionOf(annotations: Seq[Any]): Option[String] =
    annotations.collectFirst { case d: description => d.text }

  private def nameOf(annotations: Seq[Any], default: String): String =
    annotations.collectFirst { case n: name => n.value }.getOrElse(default)

  def join[A](ctx: CaseClass[Schema, A]): Schema[A] =
    if (ctx.isObject) Derive.singleton(nameOf(ctx.annotations, ctx.typeName.short), ctx.rawConstruct(Nil))
    else
      Derive.product[A](
        descriptionOf(ctx.annotations),
        ctx.parameters.toList.map { p =>
          Derive.FieldInfo[A](
            nameOf(p.annotations, p.label),
            descriptionOf(p.annotations),
            p.typeclass.asInstanceOf[Schema[Any]],
            (a: A) => p.dereference(a)
          )
        },
        values => ctx.rawConstruct(values)
      )

  def split[A](ctx: SealedTrait[Schema, A]): Schema[A] =
    Derive.sum[A](
      ctx.annotations.collectFirst { case d: discriminator => d.field }.getOrElse("type"),
      descriptionOf(ctx.annotations),
      ctx.subtypes.toList.map { s =>
        Derive.VariantInfo[A](
          nameOf(s.annotations, s.typeName.short),
          s.typeclass.asInstanceOf[Schema[A]],
          (a: A) => s.cast.isDefinedAt(a)
        )
      }
    )

  /**
   * Derives a [[Schema]] for a case class or sealed trait, including every type nested inside it.
   *
   * It is implicit, so such types have a schema without declaring one. It has the lowest priority: an
   * instance you define yourself always wins. Call it explicitly to cache an instance in a companion:
   * `implicit val schema: Schema[Book] = Schema.derived`.
   */
  // Magnolia only descends into nested types when the macro it is called through is implicit, which is
  // why one method serves both the automatic and the explicit use.
  implicit def derived[A]: Schema[A] = macro Magnolia.gen[A]
}
