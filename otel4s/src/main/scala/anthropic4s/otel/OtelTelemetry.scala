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

package anthropic4s.otel

import anthropic4s._
import cats.effect.kernel.{Ref, Resource, Temporal}
import cats.effect.kernel.Resource.ExitCase
import cats.syntax.all._
import cats.~>
import io.circe.syntax._
import org.typelevel.otel4s.trace.{Span, SpanContext, SpanKind, StatusCode, Tracer, TracerProvider}
import org.typelevel.otel4s.{Attribute, Attributes}

/** Whether prompts and replies are put on spans. */
sealed trait ContentMode extends Product with Serializable
object ContentMode {

  /** Model, token counts, timings and ids only. Nothing a user typed or the model wrote. */
  case object MetadataOnly extends ContentMode

  /**
   * Also the system prompt, messages, tool arguments and tool results. Treat such traces as sensitive data.
   */
  case object Content extends ContentMode
}

/**
 * Tracing settings.
 * {{{
 * OtelConfig.default.withContentMode(ContentMode.Content).withMaxContentBytes(64 * 1024)
 * }}}
 */
final case class OtelConfig private (contentMode: ContentMode, maxContentBytes: Int, propagation: Boolean) {
  def withContentMode(mode: ContentMode): OtelConfig = copy(contentMode = mode)

  /**
   * Upper bound for each content attribute; longer values are cut and `anthropic.content.truncated` is set.
   */
  def withMaxContentBytes(n: Int): OtelConfig = copy(maxContentBytes = math.max(0, n))

  /** Whether to send the trace context (`traceparent`, ...) to the API. On by default. */
  def withPropagation(on: Boolean): OtelConfig = copy(propagation = on)
}
object OtelConfig {
  val default: OtelConfig = new OtelConfig(ContentMode.MetadataOnly, 768 * 1024, propagation = true)
  @scala.annotation.unused
  private def unapply(c: OtelConfig): Some[OtelConfig] = Some(c)
}

/**
 * OpenTelemetry tracing for the SDK, built on otel4s.
 * {{{
 * for {
 *   telemetry <- OtelTelemetry[IO](tracerProvider)
 *   client     = AnthropicClient.fromClient(http, config, telemetry)   // or Anthropic4s.resource(config, telemetry)
 * } yield client
 * }}}
 *
 * Spans follow the contract of the official SDKs:
 *
 *   - one `CLIENT` span per API request, retries included: `anthropic.messages.create`,
 *     `anthropic.messages.count_tokens`, `anthropic.models.list`, ...
 *   - one `INTERNAL` span `anthropic.messages.tool_runner` per agent run, with the requests it makes and one
 *     `anthropic.custom_tool_use <tool>` span per tool call as its children
 *   - a streaming request's span ends when the stream is fully read or abandoned
 *
 * Token usage, model, stop reason and request ids are recorded as `gen_ai.*` and `anthropic.*` attributes.
 * Prompt and reply text is recorded only with [[ContentMode.Content]].
 */
object OtelTelemetry {

  /** The instrumentation scope name of every span this module creates. */
  val ScopeName: String = "anthropic4s"

  def apply[F[_]: Temporal](tracerProvider: TracerProvider[F], config: OtelConfig): F[Telemetry[F]] =
    tracerProvider.tracer(ScopeName).get.map(tracer => new Impl[F](tracer, config))

  def apply[F[_]: Temporal](tracerProvider: TracerProvider[F]): F[Telemetry[F]] =
    apply(tracerProvider, OtelConfig.default)

  private val Provider = Attribute("gen_ai.provider.name", "anthropic")

  final private class Impl[F[_]](tracer: Tracer[F], config: OtelConfig)(implicit F: Temporal[F])
      extends Telemetry[F] {

    private val withContent: Boolean = config.contentMode == ContentMode.Content

    /** A content attribute, cut to the configured size. The second element says whether it was cut. */
    private def content(key: String, value: String): List[Attribute[_]] = {
      val bytes = value.getBytes("UTF-8")
      if (bytes.length <= config.maxContentBytes) List(Attribute(key, value))
      else
        List(
          Attribute(key, new String(bytes, 0, config.maxContentBytes, "UTF-8")),
          Attribute("anthropic.content.truncated", true)
        )
    }

    private def requestAttributes(ctx: RequestContext): List[Attribute[_]] = {
      val base = List[Attribute[_]](
        Provider,
        Attribute("http.request.method", ctx.method),
        Attribute("http.route", ctx.route),
        Attribute("url.full", ctx.url)
      ) ++ ctx.options.conversationId.map(Attribute("gen_ai.conversation.id", _))

      // Only messages.create is a model call; token counting and the other endpoints carry no gen_ai request data.
      val message = ctx.request.filter(_ => ctx.operation == "messages.create").toList.flatMap { req =>
        List[Attribute[_]](
          Attribute("gen_ai.operation.name", "chat"),
          Attribute("gen_ai.request.model", req.model.value),
          Attribute("gen_ai.request.max_tokens", req.maxTokens.toLong),
          Attribute("gen_ai.request.stream", ctx.streaming),
          Attribute(
            "gen_ai.output.type",
            if (req.outputConfig.flatMap(_.format).isDefined) "json" else "text"
          )
        ) ++
          req.outputConfig.flatMap(_.effort).map(e => Attribute("gen_ai.request.reasoning.level", e.value)) ++
          req.thinking.map(t => Attribute("anthropic.thinking.type", t.tpe)) ++
          (if (req.stopSequences.nonEmpty && withContent)
             List(Attribute("gen_ai.request.stop_sequences", req.stopSequences: Seq[String]))
           else Nil) ++
          (if (ctx.options.betas.nonEmpty)
             List(Attribute("anthropic.beta", ctx.options.betas.map(_.value).toList.sorted: Seq[String]))
           else Nil) ++
          (if (withContent)
             req.system.toList.flatMap(s => content("gen_ai.system_instructions", s.asJson.noSpaces)) ++
               content("gen_ai.input.messages", req.messages.asJson.noSpaces)
           else Nil)
      }
      base ++ message
    }

    private def replyAttributes(message: Message): List[Attribute[_]] =
      List[Attribute[_]](
        Attribute("gen_ai.response.id", message.id),
        Attribute("gen_ai.response.model", message.model.value),
        // By convention the input count includes cached tokens; the cache split is reported separately.
        Attribute("gen_ai.usage.input_tokens", message.usage.totalInputTokens),
        Attribute("gen_ai.usage.output_tokens", message.usage.outputTokens),
        Attribute("gen_ai.usage.cache_read.input_tokens", message.usage.cacheReadInputTokens),
        Attribute("gen_ai.usage.cache_write.input_tokens", message.usage.cacheCreationInputTokens)
      ) ++
        message.stopReason.toList.flatMap { r =>
          List[Attribute[_]](
            Attribute("anthropic.message.stop_reason", r.value),
            Attribute("gen_ai.response.finish_reasons", Seq(r.value))
          )
        } ++
        (if (withContent) content("gen_ai.output.messages", List(message.toParam).asJson.noSpaces) else Nil)

    private def errorType(e: Throwable): String = e match {
      case api: AnthropicError.Api         => api.errorType
      case _: AnthropicError.Timeout       => "client_timeout_error"
      case _: AnthropicError.Connection    => "connection_error"
      case s: AnthropicError.StreamFailure => s.errorType
      case _: AnthropicError               => "client_error"
      case other                           => other.getClass.getName
    }

    /**
     * Opens a span. Passing a parent pins it explicitly; otherwise the ambient span (if any) is the parent.
     */
    private def span(
        name: String,
        kind: SpanKind,
        attributes: List[Attribute[_]],
        parent: Option[SpanContext]
    ): Resource[F, (Span[F], F ~> F)] = {
      val builder =
        tracer.spanBuilder(name).withSpanKind(kind).addAttributes(Attributes.fromSpecific(attributes))
      for {
        res <- parent.fold(builder)(builder.withParent(_)).build.resource
        // Registered after the span, so it runs before the span ends and can still annotate it.
        _ <- Resource.makeCase(F.unit) {
          case (_, ExitCase.Errored(e)) => res.span.addAttribute(Attribute("error.type", errorType(e)))
          case _                        => F.unit
        }
      } yield (res.span, res.trace)
    }

    private def requestScope(ctx: RequestContext, parent: Option[SpanContext]): Resource[F, RequestScope[F]] =
      for {
        opened <- span(s"anthropic.${ctx.operation}", SpanKind.Client, requestAttributes(ctx), parent)
        (sp, trace) = opened
        started <- Resource.eval(F.monotonic)
        acc     <- Resource.eval(Ref.of[F, MessageAccumulator](MessageAccumulator.empty))
        first   <- Resource.eval(Ref.of[F, Boolean](true))
      } yield new RequestScope[F] {

        val headers: F[List[(String, String)]] =
          if (config.propagation) trace(tracer.propagate(Map.empty[String, String])).map(_.toList)
          else F.pure(Nil)

        def attempt(number: Int): F[Unit] =
          if (number > 1) sp.addAttribute(Attribute("http.request.resend_count", (number - 1).toLong))
          else F.unit

        def response(meta: ResponseMeta): F[Unit] =
          sp.addAttributes(
            List[Attribute[_]](Attribute("http.response.status_code", meta.status.toLong)) ++
              meta.requestId.map(Attribute("anthropic.request.id", _))
          )

        def completed(message: Message): F[Unit] = sp.addAttributes(replyAttributes(message))

        def streamEvent(event: StreamEvent): F[Unit] = {
          val firstChunk = event match {
            case _: StreamEvent.ContentBlockDelta =>
              first.getAndSet(false).flatMap {
                case true =>
                  F.monotonic.flatMap(now =>
                    sp.addAttribute(
                      Attribute("gen_ai.response.time_to_first_chunk", (now - started).toNanos / 1e9)
                    )
                  )
                case false => F.unit
              }
            case _ => F.unit
          }
          // The accumulated message supplies the same attributes a non-streaming reply would.
          val accumulate = acc.modify(a => a.add(event).fold(_ => (a, a), next => (next, next))).flatMap { a =>
            if (event == StreamEvent.MessageStop) a.result.fold(_ => F.unit, completed) else F.unit
          }
          firstChunk *> accumulate
        }

        val scope: F ~> F = trace
      }

    def request(context: RequestContext): Resource[F, RequestScope[F]] = requestScope(context, None)

    def runner(context: RunnerContext): Resource[F, RunnerScope[F]] = {
      val attributes = List[Attribute[_]](
        Provider,
        Attribute("gen_ai.operation.name", "invoke_agent"),
        Attribute("gen_ai.request.model", context.request.model.value),
        Attribute("anthropic.tool_runner.tools", context.toolNames: Seq[String])
      )
      span("anthropic.messages.tool_runner", SpanKind.Internal, attributes, None).map {
        case (runnerSpan, _) =>
          // Children name their parent explicitly: the ambient context does not survive between the pulls
          // of a stream, so it cannot be relied on inside the runner.
          val parent = Some(runnerSpan.context)
          new RunnerScope[F] {
            def request(ctx: RequestContext): Resource[F, RequestScope[F]] = requestScope(ctx, parent)

            def tool(call: ContentBlock.ToolUse): Resource[F, ToolScope[F]] = {
              val attrs = List[Attribute[_]](
                Provider,
                Attribute("gen_ai.operation.name", "execute_tool"),
                Attribute("gen_ai.tool.name", call.name),
                Attribute("gen_ai.tool.call.id", call.id)
              ) ++ (if (withContent) content("gen_ai.tool.call.arguments", call.input.asJson.noSpaces)
                    else Nil)

              span(s"anthropic.custom_tool_use ${call.name}", SpanKind.Internal, attrs, parent).map {
                case (toolSpan, trace) =>
                  new ToolScope[F] {
                    def finished(result: ContentBlock.ToolResult): F[Unit] = {
                      val body =
                        if (withContent)
                          content("anthropic.tool_result.content", result.content.asJson.noSpaces)
                        else Nil
                      val status =
                        if (result.isError)
                          toolSpan.setStatus(StatusCode.Error, "the tool returned an error result")
                        else F.unit
                      toolSpan.addAttributes(body) *> status
                    }
                    // Spans the handler opens become children of the tool span.
                    val scope: F ~> F = trace
                  }
              }
            }
          }
      }
    }
  }
}
