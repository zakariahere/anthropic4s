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

import io.circe.parser.parse
import io.circe.syntax._

class AccumulatorSuite extends munit.FunSuite {

  private def events(lines: String*): List[StreamEvent] =
    lines.toList.map(l => parse(l).flatMap(_.as[StreamEvent]).fold(throw _, identity))

  private val start =
    """{"type":"message_start","message":{"id":"msg_1","type":"message","role":"assistant",
      |"model":"claude-opus-5-5","content":[],"stop_reason":null,"stop_sequence":null,
      |"usage":{"input_tokens":25,"output_tokens":1,"cache_read_input_tokens":100}}}""".stripMargin

  private val toolStream = events(
    start,
    """{"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}""",
    """{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"Let me "}}""",
    """{"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"check."}}""",
    """{"type":"content_block_stop","index":0}""",
    """{"type":"content_block_start","index":1,"content_block":{"type":"tool_use","id":"toolu_1","name":"get_weather","input":{}}}""",
    """{"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"{\"location\": \"Bru"}}""",
    """{"type":"content_block_delta","index":1,"delta":{"type":"input_json_delta","partial_json":"ssels\"}"}}""",
    """{"type":"content_block_stop","index":1}""",
    """{"type":"message_delta","delta":{"stop_reason":"tool_use","stop_sequence":null},"usage":{"output_tokens":89}}""",
    """{"type":"message_stop"}"""
  )

  test("text and tool input deltas fold into the final message") {
    val m = MessageAccumulator.fold(toolStream).fold(throw _, identity)
    assertEquals(m.text, "Let me check.")
    assertEquals(m.stopReason, Some(StopReason.ToolUse))
    assertEquals(m.toolUses.map(_.input.asJson.noSpaces), List("""{"location":"Brussels"}"""))
  }

  test("cumulative usage from message_delta replaces the starting counters it mentions") {
    val u = MessageAccumulator.fold(toolStream).fold(throw _, identity).usage
    assertEquals(u.inputTokens, 25L)
    assertEquals(u.outputTokens, 89L)
    assertEquals(u.cacheReadInputTokens, 100L)
  }

  test("thinking, signature and unknown events are handled") {
    val m = MessageAccumulator
      .fold(
        events(
          start,
          """{"type":"content_block_start","index":0,"content_block":{"type":"thinking","thinking":""}}""",
          """{"type":"content_block_delta","index":0,"delta":{"type":"thinking_delta","thinking":"Hmm"}}""",
          """{"type":"content_block_delta","index":0,"delta":{"type":"signature_delta","signature":"sig"}}""",
          """{"type":"content_block_delta","index":0,"delta":{"type":"sparkle_delta","glitter":1}}""",
          """{"type":"content_block_stop","index":0}""",
          """{"type":"future_event","x":1}""",
          """{"type":"message_delta","delta":{"stop_reason":"end_turn"},"usage":{"output_tokens":3}}""",
          """{"type":"message_stop"}"""
        )
      )
      .fold(throw _, identity)
    assertEquals(
      m.content.map(_.asJson.noSpaces),
      List("""{"type":"thinking","thinking":"Hmm","signature":"sig"}""")
    )
  }

  test("a tool call cut off mid-arguments is flagged instead of silently truncated") {
    val cut = events(
      start,
      """{"type":"content_block_start","index":0,"content_block":{"type":"tool_use","id":"toolu_1","name":"write_file","input":{}}}""",
      """{"type":"content_block_delta","index":0,"delta":{"type":"input_json_delta","partial_json":"{\"path\": \"a.txt\", \"content\": \"abc"}}""",
      """{"type":"content_block_stop","index":0}""",
      """{"type":"message_delta","delta":{"stop_reason":"max_tokens"},"usage":{"output_tokens":9}}""",
      """{"type":"message_stop"}"""
    )
    val call = MessageAccumulator.fold(cut).fold(throw _, identity).toolUses.head
    assert(call.invalidInputJson.exists(_.contains("abc")))
    assert(call.input.isEmpty)
  }

  test("a stream that ends early is reported as incomplete") {
    assertEquals(MessageAccumulator.fold(toolStream.dropRight(1)), Left(AccumulatorError.Incomplete))
    assertEquals(MessageAccumulator.fold(toolStream.drop(1)), Left(AccumulatorError.MissingMessageStart))
  }

  test("the partial message is available while streaming") {
    val acc =
      toolStream.take(3).foldLeft(MessageAccumulator.empty)((a, e) => a.add(e).fold(throw _, identity))
    assertEquals(acc.partial.map(_.text), Some("Let me "))
  }
}
