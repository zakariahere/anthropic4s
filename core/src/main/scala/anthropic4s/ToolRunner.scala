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
import cats.effect.{Concurrent, Ref, Resource}
import cats.syntax.all._
import fs2.Stream

/** What to do with a tool call before it runs. Returned by [[RunnerConfig.withBeforeTool]]. */
sealed trait ToolDecision extends Product with Serializable
object ToolDecision {

  /** Run the tool. */
  case object Allow extends ToolDecision

  /** Do not run it; tell the model why so it can try something else. */
  final case class Deny(reason: String) extends ToolDecision

  /** Do not run it; answer with this result instead (a cached value, a human's answer, ...). */
  final case class Replace(result: ToolResultContent) extends ToolDecision

  /** Stop the whole run now. Nothing is sent back to the model. */
  final case class Abort(reason: String) extends ToolDecision
}

/** Why a run ended. A refusal or a token limit is an outcome to handle, not an exception. */
sealed trait RunStop extends Product with Serializable
object RunStop {

  /** The model finished its answer. */
  case object Completed extends RunStop

  /** The iteration limit was reached while the model still wanted to call tools. */
  case object MaxIterations extends RunStop

  /** The model declined. The tools of that turn were not run. */
  final case class Refused(details: Option[StopDetails]) extends RunStop

  /** The reply hit `max_tokens`. The tools of that turn were not run, as their arguments may be cut off. */
  case object MaxTokens extends RunStop

  /** A `beforeTool` hook returned [[ToolDecision.Abort]]. */
  final case class Aborted(reason: String) extends RunStop

  /** A stop reason this SDK version does not handle specially. */
  final case class Other(reason: StopReason) extends RunStop
}

/** The outcome of a run. */
final case class RunResult private[anthropic4s] (
    message: Message,
    transcript: NonEmptyList[MessageParam],
    usage: Usage,
    iterations: Int,
    stop: RunStop
) {

  /** The text of the final reply. */
  def text: String = message.text
}

/** What happens during a run, in order. */
sealed trait RunEvent extends Product with Serializable
object RunEvent {

  /** A raw event of the model's streaming reply in the given iteration (1-based). */
  final case class Model(iteration: Int, event: StreamEvent) extends RunEvent

  /** The model's complete reply for one iteration. */
  final case class TurnCompleted(iteration: Int, message: Message) extends RunEvent

  /** A tool is about to run. Only emitted for calls the `beforeTool` hook allowed. */
  final case class ToolStarted(call: ContentBlock.ToolUse) extends RunEvent

  /** The result that goes back to the model, whether the tool ran, was denied or was replaced. */
  final case class ToolFinished(call: ContentBlock.ToolUse, result: ContentBlock.ToolResult) extends RunEvent

  /** Always the last event. */
  final case class Finished(result: RunResult) extends RunEvent
}

/**
 * Settings of a [[ToolRunner]]. Start from `RunnerConfig.default[IO]` and adjust:
 * {{{
 * RunnerConfig.default[IO]
 *   .withMaxIterations(10)
 *   .withBeforeTool(call => if (call.name == "delete_file") askHuman(call) else IO.pure(ToolDecision.Allow))
 * }}}
 */
final case class RunnerConfig[F[_]] private (
    maxIterations: Int,
    maxConcurrency: Int,
    eagerInputStreaming: Boolean,
    options: RequestOptions,
    beforeTool: ContentBlock.ToolUse => F[ToolDecision],
    onToolError: (ContentBlock.ToolUse, Throwable) => F[ContentBlock.ToolResult],
    beforeRequest: (Int, MessageRequest) => F[MessageRequest]
) {

  /** The most requests one run may make (default 25). Reaching it ends the run with `MaxIterations`. */
  def withMaxIterations(n: Int): RunnerConfig[F] = copy(maxIterations = math.max(1, n))

  /** How many tool calls of one turn may run at the same time (default 8). Use 1 for strictly serial. */
  def withMaxConcurrency(n: Int): RunnerConfig[F] = copy(maxConcurrency = math.max(1, n))

  /**
   * Stream large tool arguments as they are generated (default on). Turn it off behind a proxy or an older
   * deployment that rejects the field.
   */
  def withEagerInputStreaming(on: Boolean): RunnerConfig[F] = copy(eagerInputStreaming = on)

  /** Options applied to every request of the run. */
  def withRequestOptions(o: RequestOptions): RunnerConfig[F] = copy(options = o)

  /** An approval gate: called for every tool call before it runs. */
  def withBeforeTool(f: ContentBlock.ToolUse => F[ToolDecision]): RunnerConfig[F] = copy(beforeTool = f)

  /**
   * Turns a failed tool into the result the model sees. The default reports the exception's class and
   * message, so do not put secrets in exception messages, or replace this hook.
   */
  def withOnToolError(f: (ContentBlock.ToolUse, Throwable) => F[ContentBlock.ToolResult]): RunnerConfig[F] =
    copy(onToolError = f)

  /** Edits each request before it is sent; the first argument is the iteration, starting at 1. */
  def withBeforeRequest(f: (Int, MessageRequest) => F[MessageRequest]): RunnerConfig[F] =
    copy(beforeRequest = f)
}

object RunnerConfig {
  def default[F[_]](implicit F: cats.Applicative[F]): RunnerConfig[F] =
    new RunnerConfig[F](
      maxIterations = 25,
      maxConcurrency = 8,
      eagerInputStreaming = true,
      options = RequestOptions.default,
      beforeTool = _ => F.pure(ToolDecision.Allow),
      onToolError = (call, e) =>
        F.pure(
          ContentBlock.ToolResult
            .error(call.id, s"${e.getClass.getSimpleName}: ${Option(e.getMessage).getOrElse("")}")
        ),
      beforeRequest = (_, request) => F.pure(request)
    )

  @scala.annotation.unused
  private def unapply[F[_]](c: RunnerConfig[F]): Some[RunnerConfig[F]] = Some(c)
}

/**
 * The agent loop: sends a request, runs the tools the model asks for, sends the results back, and repeats
 * until the model is done.
 * {{{
 * val runner = client.messages.toolRunner(List(weather, calendar))
 *
 * runner.run(request)                                  // IO[RunResult]: just the outcome
 * runner.turns(request)                                // Stream[IO, Message]: each model reply
 * runner.events(request)                               // Stream[IO, RunEvent]: text deltas, tool starts, ...
 * }}}
 * Every request is streamed internally, so long generations never hit the non-streaming timeout. Cancelling
 * the effect or the stream cancels the tools that are running.
 */
final class ToolRunner[F[_]] private (messages: Messages[F], tools: List[Tool[F]], config: RunnerConfig[F])(
    implicit F: Concurrent[F]
) {
  import ToolRunner._

  private val byName: Map[String, Tool[F]] = tools.map(t => t.name -> t).toMap

  def run(request: MessageRequest): F[RunResult] =
    events(request).collect { case RunEvent.Finished(result) => result }.compile.lastOrError

  def turns(request: MessageRequest): Stream[F, Message] =
    events(request).collect { case RunEvent.TurnCompleted(_, message) => message }

  def events(request: MessageRequest): Stream[F, RunEvent] = {
    val issues = tools.flatMap(_.schemaIssues)
    NonEmptyList.fromList(issues) match {
      case Some(nel) => Stream.raiseError[F](new AnthropicError.InvalidSchema(nel))
      case None      =>
        val definitions = tools.map(t =>
          if (config.eagerInputStreaming) t.definition.withEagerInputStreaming(true) else t.definition
        )
        val withTools = request.addTools(definitions: _*)
        Stream.resource(scopes(withTools)).flatMap { case (model, toolScope) =>
          loop(1, withTools, Usage.empty, model, toolScope)
        }
    }
  }

  /** The model to talk to and the way to wrap one tool execution, both attached to the run's telemetry. */
  private def scopes(
      request: MessageRequest
  ): Resource[F, (Messages[F], ContentBlock.ToolUse => Resource[F, ToolScope[F]])] =
    messages match {
      case aware: TelemetryAware[F @unchecked] =>
        aware.telemetry.runner(RunnerContext(request, tools.map(_.name))).map { runnerScope =>
          val child = new Telemetry[F] {
            def request(context: RequestContext): Resource[F, RequestScope[F]] = runnerScope.request(context)
            def runner(context: RunnerContext): Resource[F, RunnerScope[F]] = aware.telemetry.runner(context)
          }
          (aware.withTelemetry(child), runnerScope.tool(_))
        }
      case other =>
        val noop = Telemetry.noop[F]
        noop.runner(RunnerContext(request, tools.map(_.name))).map(scope => (other, scope.tool(_)))
    }

  private def loop(
      iteration: Int,
      request: MessageRequest,
      usage: Usage,
      model: Messages[F],
      toolScope: ContentBlock.ToolUse => Resource[F, ToolScope[F]]
  ): Stream[F, RunEvent] =
    Stream.eval(config.beforeRequest(iteration, request)).flatMap { prepared =>
      Stream.eval(Ref.of[F, MessageAccumulator](MessageAccumulator.empty)).flatMap { acc =>
        val modelEvents: Stream[F, RunEvent] =
          model
            .stream(prepared, config.options)
            .evalTap(event => acc.get.flatMap(a => F.fromEither(a.add(event))).flatMap(acc.set))
            .map(RunEvent.Model(iteration, _))

        modelEvents ++ Stream.eval(acc.get.flatMap(a => F.fromEither(a.result))).flatMap { reply =>
          val total = usage |+| reply.usage

          def finish(stop: RunStop): Stream[F, RunEvent] =
            Stream.emit(
              RunEvent.Finished(RunResult(reply, prepared.messages :+ reply.toParam, total, iteration, stop))
            )

          val next: Stream[F, RunEvent] = reply.stopReason match {
            case Some(StopReason.ToolUse) if reply.toolUses.nonEmpty =>
              if (iteration >= config.maxIterations) finish(RunStop.MaxIterations)
              else
                runTools(reply.toolUses, toolScope).flatMap {
                  case Left(event)           => Stream.emit(event)
                  case Right(Left(aborted))  => finish(RunStop.Aborted(aborted.reason))
                  case Right(Right(results)) =>
                    val followUp = prepared.addMessages(reply.toParam, MessageParam.user(results))
                    loop(iteration + 1, followUp, total, model, toolScope)
                }

            // A server-side tool loop paused; sending the reply back as-is lets the API continue it.
            case Some(StopReason.PauseTurn) =>
              if (iteration >= config.maxIterations) finish(RunStop.MaxIterations)
              else loop(iteration + 1, prepared.addMessages(reply.toParam), total, model, toolScope)

            case Some(StopReason.Refusal)   => finish(RunStop.Refused(reply.stopDetails))
            case Some(StopReason.MaxTokens) => finish(RunStop.MaxTokens)
            case None | Some(StopReason.EndTurn) | Some(StopReason.StopSequence) | Some(StopReason.ToolUse) =>
              finish(RunStop.Completed)
            case Some(other) => finish(RunStop.Other(other))
          }

          Stream.emit(RunEvent.TurnCompleted(iteration, reply)) ++ next
        }
      }
    }

  /**
   * Handles the calls of one turn. First every call goes through the `beforeTool` hook, one at a time and in
   * order, so an abort happens before any tool has run. Then the allowed calls run, at most `maxConcurrency`
   * at a time. Emits progress events on the left and, last, either the abort or all results in the original
   * order (as one list, for one user message).
   */
  private def runTools(
      calls: List[ContentBlock.ToolUse],
      toolScope: ContentBlock.ToolUse => Resource[F, ToolScope[F]]
  ): Stream[F, Either[RunEvent, Either[Aborted, List[ContentBlock]]]] = {
    type Out = Either[RunEvent, Either[Aborted, List[ContentBlock]]]

    def event(e: RunEvent): Out = e.asLeft

    /** The decisions for all calls, or the first abort. Stops asking as soon as one aborts. */
    def decide(
        remaining: List[ContentBlock.ToolUse],
        decided: Vector[(ContentBlock.ToolUse, ToolDecision)]
    ): F[Either[Aborted, Vector[(ContentBlock.ToolUse, ToolDecision)]]] =
      remaining match {
        case Nil          => F.pure(decided.asRight)
        case call :: rest =>
          config.beforeTool(call).flatMap {
            case ToolDecision.Abort(reason) => F.pure(Aborted(reason).asLeft)
            case decision                   => decide(rest, decided :+ (call -> decision))
          }
      }

    def execute(call: ContentBlock.ToolUse, decision: ToolDecision): F[ContentBlock.ToolResult] =
      toolScope(call).use { scope =>
        val result: F[ContentBlock.ToolResult] = decision match {
          case ToolDecision.Deny(reason)    => F.pure(ContentBlock.ToolResult.error(call.id, reason))
          case ToolDecision.Replace(result) => F.pure(ContentBlock.ToolResult(call.id, result))
          case _                            =>
            byName.get(call.name) match {
              case Some(tool) => scope.scope(tool.run(call).handleErrorWith(config.onToolError(call, _)))
              case None       => F.pure(ContentBlock.ToolResult.error(call.id, s"Unknown tool: ${call.name}"))
            }
        }
        result.flatTap(scope.finished)
      }

    Stream.eval(decide(calls, Vector.empty)).flatMap {
      case Left(aborted)  => Stream.emit(aborted.asLeft[List[ContentBlock]].asRight[RunEvent])
      case Right(decided) =>
        val started: Stream[F, Out] =
          Stream.emits(decided.collect { case (call, ToolDecision.Allow) =>
            event(RunEvent.ToolStarted(call))
          })

        Stream.eval(Ref.of[F, Vector[ContentBlock]](Vector.empty)).flatMap { results =>
          val finished: Stream[F, Out] =
            Stream
              .emits(decided)
              .covary[F]
              .parEvalMap(config.maxConcurrency) { case (call, decision) =>
                execute(call, decision).map(result => (call, result))
              }
              .evalMap { case (call, result) =>
                results.update(_ :+ result).as(event(RunEvent.ToolFinished(call, result)))
              }
          val done: Stream[F, Out] =
            Stream.eval(results.get.map(rs => rs.toList.asRight[Aborted].asRight[RunEvent]))

          started ++ finished ++ done
        }
    }
  }
}

object ToolRunner {

  def apply[F[_]: Concurrent](
      messages: Messages[F],
      tools: List[Tool[F]],
      config: RunnerConfig[F]
  ): ToolRunner[F] =
    new ToolRunner[F](messages, tools, config)

  def apply[F[_]: Concurrent](messages: Messages[F], tools: List[Tool[F]]): ToolRunner[F] =
    new ToolRunner[F](messages, tools, RunnerConfig.default[F])

  final private case class Aborted(reason: String)
}
