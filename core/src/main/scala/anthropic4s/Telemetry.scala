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

import cats.arrow.FunctionK
import cats.effect.Resource
import cats.{~>, Applicative}

/** What the SDK is about to send. Handed to [[Telemetry]] so it can start a span. */
final case class RequestContext private[anthropic4s] (
    operation: String,
    method: String,
    route: String,
    url: String,
    request: Option[MessageRequest],
    streaming: Boolean,
    options: RequestOptions
)

final case class ResponseMeta private[anthropic4s] (
    status: Int,
    requestId: Option[String],
    headers: List[(String, String)]
)

final case class RunnerContext private[anthropic4s] (request: MessageRequest, toolNames: List[String])

/**
 * The lifetime of one API request, retries included. The surrounding `Resource` closes when the response has
 * been read (or, for a stream, fully consumed or abandoned), which is when a span should end.
 */
trait RequestScope[F[_]] {

  /** Headers to add to the outgoing request, e.g. W3C `traceparent`. Asked once per attempt. */
  def headers: F[List[(String, String)]]

  /** Called before each HTTP attempt, starting at 1. */
  def attempt(number: Int): F[Unit]

  /** Called when response headers arrive, for error statuses too. */
  def response(meta: ResponseMeta): F[Unit]

  /** Called for every event of a streaming response. */
  def streamEvent(event: StreamEvent): F[Unit]

  /** Called with the reply of a non-streaming Messages request. */
  def completed(message: Message): F[Unit]

  /** Runs an effect "inside" this request, so that work it starts is attributed to it. */
  def scope: F ~> F
}

/** The lifetime of one tool execution inside a tool runner. */
trait ToolScope[F[_]] {
  def finished(result: ContentBlock.ToolResult): F[Unit]
  def scope: F ~> F
}

/** The lifetime of one tool-runner run. Requests and tool calls made by the runner are its children. */
trait RunnerScope[F[_]] {
  def request(context: RequestContext): Resource[F, RequestScope[F]]
  def tool(call: ContentBlock.ToolUse): Resource[F, ToolScope[F]]
}

/**
 * The SDK's observability hook. The core library reports what happens; an implementation decides what to
 * record. `anthropic4s-otel4s` turns these callbacks into OpenTelemetry spans; the default does nothing.
 */
trait Telemetry[F[_]] {
  def request(context: RequestContext): Resource[F, RequestScope[F]]
  def runner(context: RunnerContext): Resource[F, RunnerScope[F]]
}

object Telemetry {

  def noop[F[_]](implicit F: Applicative[F]): Telemetry[F] = new Telemetry[F] {
    private val requestScope: RequestScope[F] = new RequestScope[F] {
      val headers: F[List[(String, String)]]       = F.pure(Nil)
      def attempt(number: Int): F[Unit]            = F.unit
      def response(meta: ResponseMeta): F[Unit]    = F.unit
      def streamEvent(event: StreamEvent): F[Unit] = F.unit
      def completed(message: Message): F[Unit]     = F.unit
      val scope: F ~> F                            = FunctionK.id[F]
    }
    private val toolScope: ToolScope[F] = new ToolScope[F] {
      def finished(result: ContentBlock.ToolResult): F[Unit] = F.unit
      val scope: F ~> F                                      = FunctionK.id[F]
    }
    private val runnerScope: RunnerScope[F] = new RunnerScope[F] {
      def request(context: RequestContext): Resource[F, RequestScope[F]] = Resource.pure(requestScope)
      def tool(call: ContentBlock.ToolUse): Resource[F, ToolScope[F]]    = Resource.pure(toolScope)
    }
    def request(context: RequestContext): Resource[F, RequestScope[F]] = Resource.pure(requestScope)
    def runner(context: RunnerContext): Resource[F, RunnerScope[F]]    = Resource.pure(runnerScope)
  }
}
