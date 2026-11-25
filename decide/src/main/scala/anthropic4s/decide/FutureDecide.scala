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
import cats.effect.unsafe.IORuntime
import io.circe.Json

import scala.concurrent.Future

/**
 * The decision client for code that works with `Future` instead of `IO`, such as a Play application.
 * {{{
 * @Singleton
 * class Triage @Inject() (lifecycle: ApplicationLifecycle)(implicit ec: ExecutionContext) {
 *   private val decide = FutureDecide.local()
 *   lifecycle.addStopHook(() => decide.close())
 *
 *   def team(ticket: String): Future[ChoiceAnswer[Team]] = decide.ask(ticket, Questions.team)
 * }
 * }}}
 * Create one instance for the whole application and close it on shutdown. Failures arrive as failed futures
 * holding a [[DecideError]]; use `recover` to handle them.
 *
 * Several questions go in one request as a batch, combined with cats syntax:
 * {{{
 * import cats.syntax.all._
 * decide.run(ticket, (team.as("team"), angry.as("angry")).tupled) // Future[(ChoiceAnswer[Team], Double)]
 * }}}
 */
final class FutureDecide private (val client: Decide[IO], shutdown: IO[Unit])(implicit runtime: IORuntime) {

  /** Runs any decision effect and returns its result as a `Future`. */
  def run[A](effect: IO[A]): Future[A] = effect.unsafeToFuture()

  /** One question, one request. */
  def ask[A](state: String, question: Question[A]): Future[A] = run(client.ask(state, question))
  def ask[A](state: Json, question: Question[A]): Future[A]   = run(client.ask(state, question))

  /** Every question of the batch in one request. */
  def run[A](state: String, batch: Batch[A]): Future[A] = run(client.run(state, batch))
  def run[A](state: Json, batch: Batch[A]): Future[A]   = run(client.run(state, batch))

  /** Closes the HTTP connections. Call it once, when the application stops. */
  def close(): Future[Unit] = run(shutdown)
}

object FutureDecide {

  /** Starts a client. Building it opens no connection; that happens on first use. */
  def start(config: DecideConfig)(implicit runtime: IORuntime = IORuntime.global): FutureDecide = {
    val (client, shutdown) = Decide.ember[IO](config).allocated.unsafeRunSync()
    new FutureDecide(client, shutdown)
  }

  /** Starts a client for llama-server on this machine. */
  def local(port: Int = 8081)(implicit runtime: IORuntime = IORuntime.global): FutureDecide =
    start(DecideConfig.local(port))

  /**
   * Starts a client configured from `DECIDE_BASE_URL`, `DECIDE_API_KEY` and `DECIDE_TIMEOUT`, all optional:
   * with none set, it is the same as [[local]]. Throws [[DecideError.Configuration]] for a value it cannot
   * read, so a bad deployment fails at startup.
   */
  def fromEnv()(implicit runtime: IORuntime = IORuntime.global): FutureDecide =
    start(DecideConfig.fromEnv[IO].unsafeRunSync())

  /** Wraps a client you built yourself (on your own HTTP client, say). */
  def wrap(client: Decide[IO], shutdown: IO[Unit])(implicit
      runtime: IORuntime = IORuntime.global
  ): FutureDecide =
    new FutureDecide(client, shutdown)
}
