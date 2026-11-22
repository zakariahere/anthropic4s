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

import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.all._
import io.circe.{Decoder, DecodingFailure, Encoder, Json, JsonObject}

import java.time.{Instant, LocalDate}
import java.util.UUID

/**
 * Everything needed to exchange a value of type `A` with the model as JSON: its JSON Schema, a decoder for
 * what comes back and an encoder for sending it.
 *
 * Case classes and sealed traits get one automatically, and all three parts agree by construction:
 * {{{
 * final case class Book(@description("Full title") title: String, year: Option[Int], genre: Genre)
 *
 * client.messages.parse[Book](request)   // no instance to declare
 * }}}
 * To avoid deriving the same instance at every use, cache it:
 * `object Book { implicit val schema: Schema[Book] = Schema.derived }`.
 *
 *   - a case class becomes a closed object with every field required
 *   - `Option[A]` becomes "the value or `null`" (the field is still required, as strict mode demands)
 *   - a sealed trait of case classes becomes `anyOf`, tagged by a `type` field
 *   - a sealed trait of case objects becomes a string enum
 *
 * Recursive types and `Map` are not supported: strict mode cannot express them.
 */
trait Schema[A] { self =>
  def node: SchemaNode
  def decoder: Decoder[A]
  def encoder: Encoder[A]

  /** The JSON Schema document. */
  def jsonSchema: JsonObject = SchemaNode.toJson(node)

  /** The schema, or the reasons it cannot be used for structured outputs or a strict tool. */
  def strict: ValidatedNel[SchemaIssue, JsonObject] = Schema.check(node).map(_ => jsonSchema)

  /**
   * Adds a rule the schema itself cannot express (a range, a pattern, ...). The rule is appended to nothing
   * on the wire; it is enforced when decoding, so say it in a `@description` as well.
   */
  def emap[B](f: A => Either[String, B])(g: B => A): Schema[B] =
    Schema.instance(node, decoder.emap(f), encoder.contramap(g))

  def imap[B](f: A => B)(g: B => A): Schema[B] = Schema.instance(node, decoder.map(f), encoder.contramap(g))

  /** Swaps the codec while keeping the schema. The new codec must read and write the same JSON shape. */
  def withCodec(d: Decoder[A], e: Encoder[A]): Schema[A] = Schema.instance(node, d, e)
}

object Schema extends SchemaInstances {

  def apply[A](implicit schema: Schema[A]): Schema[A] = schema

  def instance[A](n: SchemaNode, d: Decoder[A], e: Encoder[A]): Schema[A] = new Schema[A] {
    val node: SchemaNode    = n
    val decoder: Decoder[A] = d
    val encoder: Encoder[A] = e
  }

  /**
   * A hand-written JSON Schema paired with your own codec. Nothing checks that they agree, or that the schema
   * is valid in strict mode; that is on you.
   */
  def unchecked[A](schema: JsonObject)(implicit d: Decoder[A], e: Encoder[A]): Schema[A] =
    instance(SchemaNode.Raw(schema), d, e)

  private[schema] def check(root: SchemaNode): ValidatedNel[SchemaIssue, Unit] = {
    def walk(node: SchemaNode, path: String): List[SchemaIssue] = node match {
      case SchemaNode.Obj(fields, _)           => fields.flatMap(f => walk(f.schema, s"$path.${f.name}"))
      case SchemaNode.OneOf(disc, variants, _) =>
        val collisions = variants.flatMap(v =>
          v.fields.filter(_.name == disc).map(_ => SchemaIssue.DiscriminatorCollision(v.tag, disc))
        )
        val duplicates =
          variants.groupBy(_.tag).collect { case (tag, vs) if vs.size > 1 => SchemaIssue.DuplicateTag(tag) }
        collisions ++ duplicates ++ variants.flatMap(v =>
          v.fields.flatMap(f => walk(f.schema, s"$path.${f.name}"))
        )
      case SchemaNode.Enum(Nil)       => List(SchemaIssue.EmptyEnum(path))
      case SchemaNode.Arr(items)      => walk(items, s"$path[]")
      case SchemaNode.Nullable(inner) => walk(inner, path)
      case _                          => Nil
    }
    val rootIssue = root match {
      case _: SchemaNode.Obj | _: SchemaNode.Raw | _: SchemaNode.Singleton => Nil
      case _                                                               => List(SchemaIssue.RootNotObject)
    }
    NonEmptyList.fromList(rootIssue ++ walk(root, "$")).toInvalid(())
  }
}

// Extending the derivation trait gives these instances priority over automatic derivation.
private[schema] trait SchemaInstances extends SchemaDerivation {
  import SchemaNode._

  private def primitive[A](node: SchemaNode)(implicit d: Decoder[A], e: Encoder[A]): Schema[A] =
    Schema.instance(node, d, e)

  implicit val stringSchema: Schema[String]         = primitive(Str(None))
  implicit val intSchema: Schema[Int]               = primitive(Integer)
  implicit val longSchema: Schema[Long]             = primitive(Integer)
  implicit val bigIntSchema: Schema[BigInt]         = primitive(Integer)
  implicit val doubleSchema: Schema[Double]         = primitive(Number)
  implicit val floatSchema: Schema[Float]           = primitive(Number)
  implicit val bigDecimalSchema: Schema[BigDecimal] = primitive(Number)
  implicit val booleanSchema: Schema[Boolean]       = primitive(Bool)
  implicit val uuidSchema: Schema[UUID]             = primitive(Str(Some("uuid")))
  implicit val instantSchema: Schema[Instant]       = primitive(Str(Some("date-time")))
  implicit val localDateSchema: Schema[LocalDate]   = primitive(Str(Some("date")))

  implicit def optionSchema[A](implicit a: Schema[A]): Schema[Option[A]] =
    Schema.instance(Nullable(a.node), Decoder.decodeOption(a.decoder), Encoder.encodeOption(a.encoder))

  implicit def listSchema[A](implicit a: Schema[A]): Schema[List[A]] =
    Schema.instance(Arr(a.node), Decoder.decodeList(a.decoder), Encoder.encodeList(a.encoder))

  implicit def vectorSchema[A](implicit a: Schema[A]): Schema[Vector[A]] =
    Schema.instance(Arr(a.node), Decoder.decodeVector(a.decoder), Encoder.encodeVector(a.encoder))

  implicit def setSchema[A](implicit a: Schema[A]): Schema[Set[A]] =
    Schema.instance(Arr(a.node), Decoder.decodeSet(a.decoder), Encoder.encodeSet(a.encoder))

  implicit def nonEmptyListSchema[A](implicit a: Schema[A]): Schema[NonEmptyList[A]] =
    Schema.instance(Arr(a.node), Decoder.decodeNonEmptyList(a.decoder), Encoder.encodeNonEmptyList(a.encoder))
}

/**
 * The derivation logic, free of any macro library. The Scala 2 and Scala 3 glue only translate what the
 * compiler knows about a type into these calls.
 */
private[schema] object Derive {

  final case class FieldInfo[A](name: String, description: Option[String], schema: Schema[Any], get: A => Any)

  def singleton[A](tag: String, value: A): Schema[A] =
    Schema.instance(SchemaNode.Singleton(tag), Decoder.const(value), Encoder.instance(_ => Json.obj()))

  def product[A](
      description: Option[String],
      fields: List[FieldInfo[A]],
      construct: List[Any] => A
  ): Schema[A] = {
    val node =
      SchemaNode.Obj(fields.map(f => SchemaNode.Field(f.name, f.schema.node, f.description)), description)

    val decoder: Decoder[A] = Decoder.instance { c =>
      fields.traverse(f => c.downField(f.name).as(f.schema.decoder)).map(construct)
    }
    val encoder: Encoder[A] = Encoder.instance { a =>
      Json.fromFields(fields.map(f => f.name -> f.schema.encoder(f.get(a))))
    }
    Schema.instance(node, decoder, encoder)
  }

  final case class VariantInfo[A](tag: String, schema: Schema[A], matches: A => Boolean)

  def sum[A](
      discriminator: String,
      description: Option[String],
      variants: List[VariantInfo[A]]
  ): Schema[A] = {
    val allSingletons = variants.nonEmpty && variants.forall(_.schema.node.isInstanceOf[SchemaNode.Singleton])

    def variantOf(a: A): VariantInfo[A] =
      variants
        .find(_.matches(a))
        .getOrElse(throw new IllegalStateException(s"no case of the sealed trait matches $a"))

    if (allSingletons) {
      val decoder: Decoder[A] = Decoder.instance { c =>
        c.as[String].flatMap { tag =>
          variants
            .find(_.tag == tag)
            .toRight(
              DecodingFailure(s"'$tag' is not one of ${variants.map(_.tag).mkString(", ")}", c.history)
            )
            .flatMap(_.schema.decoder.decodeJson(Json.obj()))
        }
      }
      Schema.instance(
        SchemaNode.Enum(variants.map(_.tag)),
        decoder,
        Encoder.instance(a => Json.fromString(variantOf(a).tag))
      )
    } else {
      val nodeVariants = variants.map { v =>
        v.schema.node match {
          case SchemaNode.Obj(fields, d) => SchemaNode.Variant(v.tag, fields, d)
          case _                         => SchemaNode.Variant(v.tag, Nil, None)
        }
      }
      val decoder: Decoder[A] = Decoder.instance { c =>
        c.get[String](discriminator).flatMap { tag =>
          variants
            .find(_.tag == tag)
            .toRight(
              DecodingFailure(s"'$tag' is not one of ${variants.map(_.tag).mkString(", ")}", c.history)
            )
            .flatMap(_.schema.decoder(c))
        }
      }
      val encoder: Encoder[A] = Encoder.instance { a =>
        val v = variantOf(a)
        v.schema.encoder(a).mapObject(obj => (discriminator -> Json.fromString(v.tag)) +: obj)
      }
      Schema.instance(SchemaNode.OneOf(discriminator, nodeVariants, description), decoder, encoder)
    }
  }
}
