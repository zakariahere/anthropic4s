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

import cats.data.Validated
import io.circe.Json
import io.circe.parser.parse

sealed trait Genre
object Genre {
  case object Fiction                  extends Genre
  @name("science") case object Science extends Genre
}

final case class Author(name: String, born: Option[Int])

sealed trait Edition
object Edition {
  final case class Paper(pages: Int)                                            extends Edition
  final case class Audio(@name("minutes") lengthMinutes: Int, narrator: String) extends Edition
  case object Unreleased                                                        extends Edition
}

@description("A book in the catalogue")
final case class Book(
    @description("Full title, without the subtitle") title: String,
    author: Author,
    genre: Genre,
    editions: List[Edition],
    year: Option[Int]
)
object Book {
  // Cached once. Author, Genre and Edition need no instance of their own: they are derived along the way.
  implicit val schema: Schema[Book] = Schema.derived
}

final case class Shelf(label: String, books: List[Book])

final case class Celsius(value: Double)
object Celsius {
  // A hand-written instance: it must win over automatic derivation.
  implicit val schema: Schema[Celsius] = Schema[Double].imap(Celsius(_))(_.value)
}
final case class Reading(temperature: Celsius)

final case class Clash(`type`: String)
sealed trait Clashing
object Clashing {
  final case class A(`type`: String) extends Clashing; final case class B(n: Int) extends Clashing
}
final case class HoldsClash(value: Clashing)

class SchemaSuite extends munit.FunSuite {

  private def json(s: String): Json = parse(s).fold(throw _, identity)

  private val sample = Book(
    "Dune",
    Author("Frank Herbert", Some(1920)),
    Genre.Science,
    List(Edition.Paper(412), Edition.Audio(1260, "Scott Brick"), Edition.Unreleased),
    None
  )

  test("a case class becomes a closed object with every field required") {
    val expected = json(
      """{
        |  "type": "object",
        |  "description": "A book in the catalogue",
        |  "additionalProperties": false,
        |  "required": ["title", "author", "genre", "editions", "year"],
        |  "properties": {
        |    "title": {"type": "string", "description": "Full title, without the subtitle"},
        |    "author": {
        |      "type": "object", "additionalProperties": false, "required": ["name", "born"],
        |      "properties": {
        |        "name": {"type": "string"},
        |        "born": {"anyOf": [{"type": "integer"}, {"type": "null"}]}
        |      }
        |    },
        |    "genre": {"type": "string", "enum": ["Fiction", "science"]},
        |    "editions": {
        |      "type": "array",
        |      "items": {"anyOf": [
        |        {"type": "object", "additionalProperties": false, "required": ["type", "minutes", "narrator"],
        |         "properties": {"type": {"type": "string", "const": "Audio"},
        |                        "minutes": {"type": "integer"}, "narrator": {"type": "string"}}},
        |        {"type": "object", "additionalProperties": false, "required": ["type", "pages"],
        |         "properties": {"type": {"type": "string", "const": "Paper"}, "pages": {"type": "integer"}}},
        |        {"type": "object", "additionalProperties": false, "required": ["type"],
        |         "properties": {"type": {"type": "string", "const": "Unreleased"}}}
        |      ]}
        |    },
        |    "year": {"anyOf": [{"type": "integer"}, {"type": "null"}]}
        |  }
        |}""".stripMargin
    )
    assertEquals(Json.fromJsonObject(Schema[Book].jsonSchema), expected)
  }

  test("what the encoder writes satisfies the schema, and decodes back to the same value") {
    val schema  = Schema[Book]
    val encoded = schema.encoder(sample)
    assertEquals(SchemaValidator.validate(schema.jsonSchema, encoded), Nil)
    assertEquals(schema.decoder.decodeJson(encoded), Right(sample))
  }

  test("an absent Option is written as an explicit null, because the field is required") {
    val encoded = Schema[Book].encoder(sample)
    assertEquals(encoded.hcursor.downField("year").focus, Some(Json.Null))
    assertEquals(encoded.hcursor.downField("author").downField("born").as[Int], Right(1920))
  }

  test("a reply written by the model decodes") {
    val reply = json(
      """{"title":"Neuromancer","author":{"name":"William Gibson","born":null},"genre":"Fiction",
        |"editions":[{"type":"Audio","minutes":630,"narrator":"Robertson Dean"}],"year":1984}""".stripMargin
    )
    val book = Schema[Book].decoder.decodeJson(reply).fold(throw _, identity)
    assertEquals(book.genre, Genre.Fiction)
    assertEquals(book.editions, List(Edition.Audio(630, "Robertson Dean")))
    assertEquals(book.author.born, None)
  }

  test("the validator rejects what the schema forbids") {
    val schema = Schema[Book].jsonSchema
    val base   = Schema[Book].encoder(sample)
    val extra  = base.mapObject(_.add("isbn", Json.fromString("x")))
    val wrong  = base.mapObject(_.add("genre", Json.fromString("Poetry")))
    val gone   = base.mapObject(_.remove("year"))
    assert(SchemaValidator.validate(schema, extra).exists(_.contains("unexpected property 'isbn'")))
    assert(SchemaValidator.validate(schema, wrong).exists(_.contains("$.genre")))
    assert(SchemaValidator.validate(schema, gone).exists(_.contains("missing required property 'year'")))
  }

  test("strict accepts a case class at the root and explains what it rejects") {
    assert(Schema[Book].strict.isValid)
    assertEquals(Schema[String].strict.swap.map(_.toList), Validated.valid(List(SchemaIssue.RootNotObject)))
    assertEquals(
      Schema[List[Book]].strict.swap.map(_.toList),
      Validated.valid(List(SchemaIssue.RootNotObject))
    )
    val clash = Schema.derived[HoldsClash].strict.swap.map(_.toList)
    assertEquals(clash, Validated.valid(List[SchemaIssue](SchemaIssue.DiscriminatorCollision("A", "type"))))
  }

  test("no instance needs declaring, and a hand-written one wins over derivation") {
    assert(Schema[Shelf].strict.isValid)
    val reading = Schema[Reading]
    assertEquals(reading.encoder(Reading(Celsius(21.5))).noSpaces, """{"temperature":21.5}""")
    assertEquals(
      reading.jsonSchema("properties").flatMap(_.hcursor.downField("temperature").focus).map(_.noSpaces),
      Some("""{"type":"number"}""")
    )
  }

  test("emap adds a rule the schema cannot express") {
    val year: Schema[Int] =
      Schema[Int].emap(y => Either.cond(y > 1400, y, s"$y is before printing"))(identity)
    assert(year.decoder.decodeJson(Json.fromInt(1984)).isRight)
    assert(year.decoder.decodeJson(Json.fromInt(900)).isLeft)
  }
}
