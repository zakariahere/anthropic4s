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

import anthropic4s.schema.Schema
import anthropic4s.syntax._
import cats.effect.IO
import cats.effect.unsafe.IORuntime

import scala.concurrent.Future

/**
 * The SDK for code that works with `Future` instead of `IO`, such as a Play application.
 * {{{
 * @Singleton
 * class Claude @Inject() (lifecycle: ApplicationLifecycle)(implicit ec: ExecutionContext) {
 *   private val anthropic = FutureAnthropic.fromEnv()
 *   lifecycle.addStopHook(() => anthropic.close())
 *
 *   def ask(question: String): Future[String] =
 *     anthropic.create(MessageRequest(Model.ClaudeOpus5_5, 16000, MessageParam.user(question))).map(_.text)
 * }
 * }}}
 * Create one instance for the whole application and close it on shutdown. Failures arrive as failed futures
 * holding an [[AnthropicError]]; use `recover` to handle them.
 *
 * Everything else the SDK offers is reachable through [[client]] and [[run]].
 */
final class FutureAnthropic private (val client: AnthropicClient[IO], shutdown: IO[Unit])(implicit
    runtime: IORuntime
) {

  /** Runs any SDK effect and returns its result as a `Future`. */
  def run[A](effect: IO[A]): Future[A] = effect.unsafeToFuture()

  def create(request: MessageRequest): Future[Message] = run(client.messages.create(request))

  def create(request: MessageRequest, options: RequestOptions): Future[Message] =
    run(client.messages.create(request, options))

  /** Structured output: the reply decoded into `A`. */
  def parse[A](request: MessageRequest)(implicit schema: Schema[A]): Future[Parsed[A]] =
    run(client.messages.parse[A](request))

  def countTokens(request: MessageRequest): Future[TokenCount] = run(client.messages.countTokens(request))

  /**
   * Streams the reply, calling `onEvent` for each event in order, and completes with the full message.
   * `onEvent` runs on the SDK's threads: keep it quick and do not block in it.
   */
  def stream(request: MessageRequest)(onEvent: StreamEvent => Unit): Future[Message] =
    run(client.messages.stream(request).evalTap(event => IO(onEvent(event))).finalMessage)

  /** Like [[stream]], but only hands over the text as it is written. */
  def streamText(request: MessageRequest)(onText: String => Unit): Future[Message] =
    stream(request) {
      case StreamEvent.ContentBlockDelta(_, Delta.Text(text)) => onText(text)
      case _                                                  => ()
    }

  /** Runs an agent loop over the given tools. Build the tools with [[FutureAnthropic.tool]]. */
  def runTools(request: MessageRequest, tools: List[Tool[IO]]): Future[RunResult] =
    run(client.messages.toolRunner(tools).run(request))

  def runTools(request: MessageRequest, tools: List[Tool[IO]], config: RunnerConfig[IO]): Future[RunResult] =
    run(client.messages.toolRunner(tools, config).run(request))

  /** Closes the HTTP connections. Call it once, when the application stops. */
  def close(): Future[Unit] = run(shutdown)
}

object FutureAnthropic {

  /** Starts a client. Building it opens no connection; that happens on first use. */
  def start(config: AnthropicConfig)(implicit runtime: IORuntime = IORuntime.global): FutureAnthropic = {
    val (client, shutdown) = Anthropic4s.resource[IO](config).allocated.unsafeRunSync()
    new FutureAnthropic(client, shutdown)
  }

  /**
   * Starts a client configured from `ANTHROPIC_API_KEY` / `ANTHROPIC_AUTH_TOKEN` and `ANTHROPIC_BASE_URL`.
   */
  def fromEnv()(implicit runtime: IORuntime = IORuntime.global): FutureAnthropic =
    start(AnthropicConfig.fromEnv[IO].unsafeRunSync())

  /** Wraps a client you built yourself (with telemetry or your own HTTP backend, say). */
  def wrap(client: AnthropicClient[IO], shutdown: IO[Unit])(implicit
      runtime: IORuntime = IORuntime.global
  ): FutureAnthropic =
    new FutureAnthropic(client, shutdown)

  /** A tool whose handler returns a `Future`. */
  def tool[In, Out](name: String, description: String)(
      f: In => Future[Out]
  )(implicit in: Schema[In], out: ToolOutput[Out]): Tool[IO] =
    Tool[IO, In, Out](name, description)(input => IO.fromFuture(IO(f(input))))
}
