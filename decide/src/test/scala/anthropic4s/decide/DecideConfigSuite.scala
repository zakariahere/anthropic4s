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

package anthropic4s.decide

import cats.effect.IO
import cats.effect.std.Env
import cats.syntax.all._
import munit.CatsEffectSuite
import org.http4s.Uri

import scala.concurrent.duration._

class DecideConfigSuite extends CatsEffectSuite {

  /** A fake environment, so the tests neither read nor need real variables. */
  private def env(vars: (String, String)*): Env[IO] = new Env[IO] {
    def get(name: String): IO[Option[String]]                              = IO.pure(vars.toMap.get(name))
    def entries: IO[scala.collection.immutable.Iterable[(String, String)]] = IO.pure(vars.toList)
  }

  private def fromEnv(vars: (String, String)*): IO[DecideConfig] =
    DecideConfig.fromEnv[IO](implicitly, env(vars: _*))

  test("with nothing set, it is llama-server on this machine") {
    fromEnv().map(config => assertEquals(config, DecideConfig.local()))
  }

  test("reads the base URL, the key and the timeout") {
    fromEnv(
      "DECIDE_BASE_URL" -> "https://decide.example.com/kev/",
      "DECIDE_API_KEY"  -> "secret",
      "DECIDE_TIMEOUT"  -> "30s"
    ).map { config =>
      assertEquals(config.endpoint, Uri.unsafeFromString("https://decide.example.com/kev/v1/systemone"))
      assertEquals(config.apiKey, Some("secret"))
      assertEquals(config.timeout, 30.seconds)
    }
  }

  test("blank values count as unset") {
    fromEnv("DECIDE_BASE_URL" -> "  ", "DECIDE_API_KEY" -> "", "DECIDE_TIMEOUT" -> " ")
      .map(config => assertEquals(config, DecideConfig.local()))
  }

  test("a timeout can be written in words") {
    fromEnv("DECIDE_TIMEOUT" -> "2 minutes").map(config => assertEquals(config.timeout, 2.minutes))
  }

  test("an unreadable URL is a configuration error naming the variable") {
    interceptIO[DecideError.Configuration](fromEnv("DECIDE_BASE_URL" -> "http://bad host")).map { e =>
      assert(e.detail.contains("DECIDE_BASE_URL"), e.detail)
    }
  }

  test("a timeout that is not a positive duration is a configuration error") {
    List("soon", "0s", "-5s", "Inf").traverse_ { value =>
      interceptIO[DecideError.Configuration](fromEnv("DECIDE_TIMEOUT" -> value)).map { e =>
        assert(e.detail.contains("DECIDE_TIMEOUT"), s"$value: ${e.detail}")
      }
    }
  }
}
