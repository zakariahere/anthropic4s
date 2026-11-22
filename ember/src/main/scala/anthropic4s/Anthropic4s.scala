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

import cats.effect.std.Env
import cats.effect.{Async, Resource}
import fs2.io.net.Network
import org.http4s.ember.client.EmberClientBuilder

import scala.concurrent.duration.Duration

/**
 * The quickest way to a working client: an [[AnthropicClient]] on top of http4s Ember.
 * {{{
 * import anthropic4s._
 * import cats.effect.{IO, IOApp}
 *
 * object Main extends IOApp.Simple {
 *   def run: IO[Unit] =
 *     Anthropic4s.fromEnv[IO].use { client =>
 *       client.messages
 *         .create(MessageRequest(Model.ClaudeOpus5_5, 16000, MessageParam.user("Hello!")))
 *         .flatMap(reply => IO.println(reply.text))
 *     }
 * }
 * }}}
 *
 * The client is a `Resource`: connections are opened inside `use` and closed when it ends. Create it once and
 * share it; it is safe to use from many fibers at the same time.
 */
object Anthropic4s {

  def resource[F[_]: Async: Network](config: AnthropicConfig): Resource[F, AnthropicClient[F]] =
    resource(config, Telemetry.noop[F])

  def resource[F[_]: Async: Network](
      config: AnthropicConfig,
      telemetry: Telemetry[F]
  ): Resource[F, AnthropicClient[F]] =
    EmberClientBuilder
      .default[F]
      // Ember's own limits (45s per request, 60s of silence) are far too short for a model that may
      // think for minutes. The SDK applies its own timeouts, so Ember's are switched off.
      .withTimeout(Duration.Inf)
      .withIdleConnectionTime(Duration.Inf)
      // A response that was not read to the end (an abandoned or cancelled stream) is closed at once.
      // By default Ember would first spend up to 5 seconds draining it in the hope of reusing the
      // connection. Fully read responses are unaffected and still go back to the pool.
      .withMaxDrainBytes(-1L)
      .build
      .map(http => AnthropicClient.fromClient[F](http, config, telemetry))

  /** A client configured from `ANTHROPIC_API_KEY` / `ANTHROPIC_AUTH_TOKEN` and `ANTHROPIC_BASE_URL`. */
  def fromEnv[F[_]: Async: Network: Env]: Resource[F, AnthropicClient[F]] =
    Resource.eval(AnthropicConfig.fromEnv[F]).flatMap(resource[F](_))
}
