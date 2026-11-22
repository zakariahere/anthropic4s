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

import cats.syntax.all._
import io.circe.parser.decode
import io.circe.syntax._

class UsageSuite extends munit.FunSuite {

  test("counters add up with the Monoid") {
    val total = List(Usage(10, 5), Usage(3, 7).withCacheReadInputTokens(100)).combineAll
    assertEquals(total.inputTokens, 13L)
    assertEquals(total.outputTokens, 12L)
    assertEquals(total.totalInputTokens, 113L)
  }

  test("unknown fields survive a round trip") {
    val json =
      """{"input_tokens":4,"output_tokens":2,"cache_creation_input_tokens":0,
        |"cache_read_input_tokens":0,"server_tool_use":{"web_search_requests":1}}""".stripMargin
    val usage = decode[Usage](json).fold(throw _, identity)
    assert(usage.extras.contains("server_tool_use"))
    assertEquals(usage.asJson, io.circe.parser.parse(json).fold(throw _, identity))
  }

  test("the constructor, copy and positional patterns are not part of the public API") {
    assert(compileErrors("new anthropic4s.Usage(1L, 2L, 0L, 0L, None, io.circe.JsonObject.empty)").nonEmpty)
    assert(compileErrors("anthropic4s.Usage(1L, 2L).copy(inputTokens = 3L)").nonEmpty)
    assert(
      compileErrors(
        "anthropic4s.Usage(1L, 2L) match { case anthropic4s.Usage(a, b, c, d, e, f) => a }"
      ).nonEmpty
    )
  }
}
