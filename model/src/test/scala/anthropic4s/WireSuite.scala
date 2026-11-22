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

import cats.data.NonEmptyList
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}

/**
 * The lossless law: decoding a wire document and encoding it again gives the same JSON (explicit nulls
 * aside), including block types and fields this SDK has never heard of.
 */
class WireSuite extends munit.FunSuite {

  private def json(s: String): Json = parse(s).fold(throw _, identity)

  private def assertLossless[A: Decoder: Encoder](raw: String)(implicit loc: munit.Location): A = {
    val original = json(raw)
    val decoded  = original.as[A].fold(throw _, identity)
    // Modeled optional fields drop an explicit null; unmodeled ones keep it. Both mean the same thing.
    assertEquals(decoded.asJson.deepDropNullValues, original.deepDropNullValues)
    decoded
  }

  private val messageJson =
    """{
      |  "id": "msg_01XFDUDYJgAACzvnptvVoYEL",
      |  "type": "message",
      |  "role": "assistant",
      |  "model": "claude-opus-5-5",
      |  "content": [
      |    {"type": "thinking", "thinking": "", "signature": "EqQBCgIYAh"},
      |    {"type": "text", "text": "Checking the weather.", "citations": null},
      |    {"type": "tool_use", "id": "toolu_01A09q90qw", "name": "get_weather", "input": {"location": "Brussels"}},
      |    {"type": "hologram_block", "payload": {"a": 1}}
      |  ],
      |  "stop_reason": "tool_use",
      |  "stop_sequence": null,
      |  "usage": {
      |    "input_tokens": 25, "output_tokens": 41,
      |    "cache_creation_input_tokens": 0, "cache_read_input_tokens": 0,
      |    "cache_creation": {"ephemeral_5m_input_tokens": 0, "ephemeral_1h_input_tokens": 0},
      |    "service_tier": "standard"
      |  },
      |  "container": null,
      |  "brand_new_field": {"x": [1, 2]}
      |}""".stripMargin

  test("a response round-trips, unknown block type and unknown field included") {
    val m = assertLossless[Message](messageJson)
    assertEquals(m.stopReason, Some(StopReason.ToolUse))
    assertEquals(m.text, "Checking the weather.")
    assertEquals(m.toolUses.map(_.name), List("get_weather"))
    assertEquals(m.usage.outputTokens, 41L)
    assert(m.content.exists { case ContentBlock.Unknown("hologram_block", _) => true; case _ => false })
    assert(m.extras.contains("brand_new_field"))
  }

  test("an unknown stop reason is kept as a value, not rejected") {
    val m = json(messageJson)
      .mapObject(_.add("stop_reason", "quantum_pause".asJson))
      .as[Message]
      .fold(throw _, identity)
    assertEquals(m.stopReason, Some(StopReason.of("quantum_pause")))
    assert(!m.stopReason.contains(StopReason.EndTurn))
  }

  private val requestJson =
    """{
      |  "model": "claude-opus-5-5",
      |  "max_tokens": 16000,
      |  "system": [{"type": "text", "text": "Be terse.", "cache_control": {"type": "ephemeral", "ttl": "1h"}}],
      |  "messages": [
      |    {"role": "user", "content": "Hi"},
      |    {"role": "assistant", "content": [{"type": "text", "text": "Hello"}]},
      |    {"role": "user", "content": [
      |      {"type": "image", "source": {"type": "base64", "media_type": "image/png", "data": "iVBOR"}},
      |      {"type": "tool_result", "tool_use_id": "toolu_1", "content": "22C", "is_error": true}
      |    ]}
      |  ],
      |  "tools": [
      |    {"name": "get_weather", "description": "Weather", "strict": true,
      |     "input_schema": {"type": "object", "properties": {"location": {"type": "string"}},
      |                      "required": ["location"], "additionalProperties": false}},
      |    {"type": "web_search_20260209", "name": "web_search", "max_uses": 3}
      |  ],
      |  "tool_choice": {"type": "auto", "disable_parallel_tool_use": true},
      |  "thinking": {"type": "adaptive", "display": "summarized"},
      |  "output_config": {"effort": "high", "task_budget": {"type": "tokens", "total": 64000}},
      |  "metadata": {"user_id": "u_1"},
      |  "stop_sequences": ["END"],
      |  "fallbacks": "default",
      |  "mcp_servers": [{"type": "url", "url": "https://example.com/mcp", "name": "ex"}]
      |}""".stripMargin

  test("a request round-trips, unmodeled parameters included") {
    val r = assertLossless[MessageRequest](requestJson)
    assertEquals(r.userId, Some("u_1"))
    assertEquals(r.fallbacks, Some(Fallbacks.Default))
    assertEquals(r.outputConfig.flatMap(_.effort), Some(Effort.High))
    assert(r.extras.contains("mcp_servers"))
  }

  test("the builder produces the documented wire shape") {
    val schema = JsonObject(
      "type"                 -> "object".asJson,
      "properties"           -> Json.obj("location" -> Json.obj("type" -> "string".asJson)),
      "required"             -> List("location").asJson,
      "additionalProperties" -> false.asJson
    )
    val request =
      MessageRequest(Model.ClaudeOpus5_5, 16000, MessageParam.user("What is the weather in Brussels?"))
        .withSystem("Be terse.")
        .withThinking(ThinkingConfig.adaptive)
        .withEffort(Effort.High)
        .addTools(
          ToolDefinition.custom("get_weather", "Weather", schema).withStrict(true),
          ToolDefinition.webSearch.withOption("max_uses", 3)
        )
        .withUserId("u_1")
        .withExtra("inference_profile", "x".asJson)

    val expected = json(
      """{
        |  "model": "claude-opus-5-5",
        |  "max_tokens": 16000,
        |  "messages": [{"role": "user", "content": "What is the weather in Brussels?"}],
        |  "system": "Be terse.",
        |  "tools": [
        |    {"name": "get_weather", "description": "Weather", "strict": true,
        |     "input_schema": {"type": "object", "properties": {"location": {"type": "string"}},
        |                      "required": ["location"], "additionalProperties": false}},
        |    {"type": "web_search_20260209", "name": "web_search", "max_uses": 3}
        |  ],
        |  "thinking": {"type": "adaptive"},
        |  "output_config": {"effort": "high"},
        |  "metadata": {"user_id": "u_1"},
        |  "inference_profile": "x"
        |}""".stripMargin
    )
    assertEquals(request.asJson, expected)
  }

  test("sampling parameters are not part of the request model") {
    assert(compileErrors("""anthropic4s.MessageRequest(anthropic4s.Model.ClaudeOpus5_5, 1,
      anthropic4s.MessageParam.user("x")).withTemperature(0.2)""").nonEmpty)
  }

  test("token counting strips generation-only fields") {
    val request = MessageRequest(Model.ClaudeOpus5_5, 1024, NonEmptyList.one(MessageParam.user("Hi")))
      .withStopSequences(List("END"))
      .withSystem("s")
    val body = CountTokensRequest.from(request).asJson.asObject.get
    assertEquals(body.keys.toSet, Set("model", "messages", "system"))
  }

  test("an assistant reply can be appended to the next request unchanged") {
    val reply = json(messageJson).as[Message].fold(throw _, identity)
    val next  = MessageRequest(Model.ClaudeOpus5_5, 1024, MessageParam.user("Weather?"))
      .addMessages(reply.toParam, MessageParam.user(ContentBlock.ToolResult("toolu_01A09q90qw", "22C")))
    val sent = next.asJson.hcursor.downField("messages").downN(1).downField("content").focus.get
    assertEquals(sent, json(messageJson).hcursor.downField("content").focus.get)
  }

  test("error bodies decode from the full envelope") {
    val e = json(
      """{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"},"request_id":"req_1"}"""
    )
      .as[ApiErrorBody]
      .fold(throw _, identity)
    assertEquals(e.errorType, "overloaded_error")
    assertEquals(e.message, "Overloaded")
    assertEquals(e.requestId, Some("req_1"))
  }

  test("batch results decode every outcome") {
    val ok =
      json(s"""{"custom_id":"a","result":{"type":"succeeded","message":$messageJson}}""").as[BatchResult]
    val ex = json("""{"custom_id":"b","result":{"type":"expired"}}""").as[BatchResult]
    assert(ok.exists(_.outcome.isInstanceOf[BatchOutcome.Succeeded]))
    assertEquals(ex.map(_.outcome), Right(BatchOutcome.Expired))
  }
}
