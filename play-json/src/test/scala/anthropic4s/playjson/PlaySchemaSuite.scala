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

import anthropic4s.schema.{description, Schema, SchemaValidator}
import io.circe.parser.parse
import play.api.libs.json._

sealed trait Priority
object Priority {
  case object Low  extends Priority
  case object High extends Priority
  implicit val format: Format[Priority] = PlaySchema.enumFormat("Low" -> Low, "High" -> High)
}

// The cases live at the top level: Json.format cannot see cases declared in the companion that calls it.
sealed trait Contact
final case class Email(address: String) extends Contact
final case class Phone(number: String)  extends Contact
object Contact {
  implicit val config: JsonConfiguration   = PlaySchema.configuration
  implicit val emailFormat: OFormat[Email] = Json.format[Email]
  implicit val phoneFormat: OFormat[Phone] = Json.format[Phone]
  implicit val format: OFormat[Contact]    = Json.format[Contact]
}

final case class Ticket(
    @description("One-line summary") title: String,
    priority: Priority,
    contact: Contact,
    tags: List[String],
    assignee: Option[String]
)
object Ticket {
  implicit val config: JsonConfiguration = PlaySchema.configuration
  implicit val format: OFormat[Ticket]   = Json.format[Ticket]
  implicit val schema: Schema[Ticket]    = PlaySchema.derived[Ticket]
}

final case class Note(text: String, pinned: Boolean)
object Note { implicit val format: OFormat[Note] = Json.format[Note] }

class PlaySchemaSuite extends munit.FunSuite {

  private val sample =
    Ticket("Printer on fire", Priority.High, Email("ops@example.com"), List("hardware"), None)

  test("what a Json.format writes satisfies the derived schema") {
    val schema  = Schema[Ticket]
    val written = schema.encoder(sample)
    assertEquals(SchemaValidator.validate(schema.jsonSchema, written), Nil)
    assertEquals(schema.decoder.decodeJson(written), Right(sample))
  }

  test("the codec really is the Play Format") {
    val viaPlay   = PlaySchema.toCirce(Json.toJson(sample))
    val viaSchema = Schema[Ticket].encoder(sample)
    assertEquals(viaSchema, viaPlay)
    assertEquals(viaSchema.hcursor.downField("contact").get[String]("type"), Right("Email"))
    assertEquals(viaSchema.hcursor.downField("assignee").focus, Some(io.circe.Json.Null))
  }

  test("a reply written by the model is read by the Play Format") {
    val reply = parse(
      """{"title":"VPN down","priority":"Low","contact":{"type":"Phone","number":"+32 2 555 01 00"},
        |"tags":[],"assignee":"zak"}""".stripMargin
    ).fold(throw _, identity)
    assertEquals(
      Schema[Ticket].decoder.decodeJson(reply),
      Right(Ticket("VPN down", Priority.Low, Phone("+32 2 555 01 00"), Nil, Some("zak")))
    )
  }

  test("a Play validation error is reported with its path") {
    val bad = parse(
      """{"title":"x","priority":"Urgent","contact":{"type":"Email","address":"a"},"tags":[],"assignee":null}"""
    )
      .fold(throw _, identity)
    val error = Schema[Ticket].decoder.decodeJson(bad).swap.map(_.getMessage).getOrElse("")
    assert(error.contains("priority") && error.contains("Urgent"), error)
  }

  test("an existing schema can be paired with a Play Format") {
    val schema = PlaySchema.from(Schema.derived[Note])
    assert(schema.strict.isValid)
    assertEquals(schema.encoder(Note("hi", pinned = true)).noSpaces, """{"text":"hi","pinned":true}""")
  }

  test("JSON values convert both ways without loss") {
    val js = Json.obj(
      "a" -> 1,
      "b" -> Json.arr(true, JsNull, "x", 1.5),
      "c" -> Json.obj("d" -> BigDecimal("12345678901234567890.123"))
    )
    assertEquals(PlaySchema.toPlay(PlaySchema.toCirce(js)), js)
  }
}
