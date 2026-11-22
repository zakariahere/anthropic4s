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

import anthropic4s.internal.{Call, Executor, Sse}
import anthropic4s.schema.Schema
import cats.effect.{Async, Concurrent}
import cats.syntax.all._
import cats.{~>, MonadThrow}
import fs2.Stream
import io.circe.syntax._
import org.http4s.Method
import org.http4s.client.Client

import scala.concurrent.duration._

/**
 * The Messages API.
 *
 * Every method comes in two forms: one taking just the request, and one taking [[RequestOptions]] for
 * per-call timeouts, retries, beta flags and headers.
 */
trait Messages[F[_]] {

  /**
   * Sends a request and waits for the whole reply.
   *
   * Requests with a large `max_tokens` can outlast the timeout; those fail fast with
   * [[AnthropicError.StreamingRequired]]. Use `stream(request).finalMessage` for them.
   */
  def create(request: MessageRequest, options: RequestOptions): F[Message]
  def create(request: MessageRequest): F[Message] = create(request, RequestOptions.default)

  /** Like `create`, with the HTTP status, headers and request id alongside the reply. */
  def createRaw(request: MessageRequest, options: RequestOptions): F[ApiResponse[Message]]

  /**
   * Sends a request and emits events as the reply is generated. Nothing is sent until the stream runs.
   * Stopping early (`take`, cancellation) closes the connection.
   *
   * With `import anthropic4s.syntax._`: `stream(request).textDeltas` and `stream(request).finalMessage`.
   */
  def stream(request: MessageRequest, options: RequestOptions): Stream[F, StreamEvent]
  def stream(request: MessageRequest): Stream[F, StreamEvent] = stream(request, RequestOptions.default)

  /** Counts the input tokens a request would use, without running it. */
  def countTokens(request: MessageRequest, options: RequestOptions): F[TokenCount]
  def countTokens(request: MessageRequest): F[TokenCount] = countTokens(request, RequestOptions.default)

  /**
   * Asks for a reply shaped like `A` and decodes it (structured outputs).
   *
   * The JSON Schema of `A` is sent as the response format, so the API guarantees a matching reply; case
   * classes and sealed traits get their [[anthropic4s.schema.Schema]] automatically.
   * {{{
   * final case class Invoice(@description("ISO 4217 code") currency: String, total: BigDecimal)
   *
   * client.messages.parse[Invoice](request).map(_.value.total)
   * }}}
   * Fails with [[AnthropicError.OutputParse]] when the model refuses or runs out of tokens, and with
   * [[AnthropicError.InvalidSchema]] (before sending anything) when `A` has no strict schema.
   */
  def parse[A](request: MessageRequest, options: RequestOptions)(implicit
      schema: Schema[A],
      F: MonadThrow[F]
  ): F[Parsed[A]] =
    F.fromEither(StructuredOutput.prepare[A](request)).flatMap(create(_, options)).flatMap { reply =>
      F.fromEither(StructuredOutput.read[A](reply))
    }

  def parse[A](request: MessageRequest)(implicit schema: Schema[A], F: MonadThrow[F]): F[Parsed[A]] =
    parse[A](request, RequestOptions.default)

  /**
   * An agent loop over these tools: the model calls them, the runner executes them and feeds the results back
   * until the model is done. See [[ToolRunner]].
   */
  def toolRunner(tools: List[Tool[F]], config: RunnerConfig[F])(implicit F: Concurrent[F]): ToolRunner[F] =
    ToolRunner(this, tools, config)

  def toolRunner(tools: List[Tool[F]])(implicit F: Concurrent[F]): ToolRunner[F] = ToolRunner(this, tools)

  def mapK[G[_]](fk: F ~> G): Messages[G] = Messages.mapK(this)(fk)
}

/** A structured-output reply: the decoded value and the message it came from (usage, stop reason, ...). */
final case class Parsed[A] private[anthropic4s] (value: A, message: Message)

/** The two halves of `messages.parse`, for when you drive the request yourself (streaming, batches). */
object StructuredOutput {

  /** Adds the JSON Schema of `A` to a request as its response format. */
  def prepare[A](
      request: MessageRequest
  )(implicit schema: Schema[A]): Either[AnthropicError, MessageRequest] =
    schema.strict.toEither
      .leftMap(issues => new AnthropicError.InvalidSchema(issues): AnthropicError)
      .map(json => request.withOutputFormat(OutputFormat.jsonSchema(json)))

  /** Decodes the reply to a request made with [[prepare]]. */
  def read[A](reply: Message)(implicit schema: Schema[A]): Either[AnthropicError, Parsed[A]] = {
    def fail(detail: String): Either[AnthropicError, Parsed[A]] = Left(
      new AnthropicError.OutputParse(detail, reply)
    )
    reply.stopReason match {
      case Some(StopReason.Refusal) =>
        fail(
          "the model declined to answer" + reply.stopDetails
            .flatMap(_.category)
            .fold("")(c => s" (category: $c)")
        )
      case Some(StopReason.MaxTokens) =>
        fail("the reply was cut off at max_tokens before the JSON was complete; raise maxTokens")
      case _ =>
        io.circe.parser.parse(reply.text) match {
          case Left(e)     => fail(s"the reply is not JSON: ${e.message}")
          case Right(json) =>
            schema.decoder.decodeJson(json) match {
              case Left(e)      => fail(s"the reply does not match the schema: ${e.getMessage}")
              case Right(value) => Right(Parsed(value, reply))
            }
        }
    }
  }
}

object Messages {
  private[anthropic4s] def mapK[F[_], G[_]](self: Messages[F])(fk: F ~> G): Messages[G] = new Messages[G] {
    def create(request: MessageRequest, options: RequestOptions): G[Message] = fk(
      self.create(request, options)
    )
    def createRaw(request: MessageRequest, options: RequestOptions): G[ApiResponse[Message]] =
      fk(self.createRaw(request, options))
    def stream(request: MessageRequest, options: RequestOptions): Stream[G, StreamEvent] =
      self.stream(request, options).translate(fk)
    def countTokens(request: MessageRequest, options: RequestOptions): G[TokenCount] =
      fk(self.countTokens(request, options))
  }
}

/** Paging parameters shared by the list endpoints. */
final case class ListParams private (limit: Option[Int], afterId: Option[String], beforeId: Option[String]) {
  def withLimit(n: Int): ListParams        = copy(limit = Some(n))
  def withAfterId(id: String): ListParams  = copy(afterId = Some(id))
  def withBeforeId(id: String): ListParams = copy(beforeId = Some(id))

  private[anthropic4s] def query: List[(String, String)] =
    limit.map(n => "limit" -> n.toString).toList ++ afterId.map("after_id" -> _) ++ beforeId.map(
      "before_id" -> _
    )
}
object ListParams {
  val default: ListParams = new ListParams(None, None, None)
  @scala.annotation.unused
  private def unapply(p: ListParams): Some[ListParams] = Some(p)
}

/** The Models API: discover which models exist and their limits. */
trait Models[F[_]] {
  def retrieve(model: Model): F[ModelInfo]
  def list(params: ListParams): F[Page[ModelInfo]]
  def list: F[Page[ModelInfo]] = list(ListParams.default)

  /** Every model, fetching further pages as the stream is consumed. */
  def listAll: Stream[F, ModelInfo]

  def mapK[G[_]](fk: F ~> G): Models[G] = {
    val self = this
    new Models[G] {
      def retrieve(model: Model): G[ModelInfo]         = fk(self.retrieve(model))
      def list(params: ListParams): G[Page[ModelInfo]] = fk(self.list(params))
      def listAll: Stream[G, ModelInfo]                = self.listAll.translate(fk)
    }
  }
}

/**
 * The entry point of the SDK.
 *
 * Most applications create one with `AnthropicClient.resource[IO](config)` from the `anthropic4s` artifact
 * and keep it for the lifetime of the application. `F` is the effect type: `IO` in a Cats Effect application.
 */
trait AnthropicClient[F[_]] {
  def messages: Messages[F]
  def models: Models[F]
  def batches: Batches[F]
  def files: Files[F]

  /**
   * The same client in another effect type, e.g. a monad transformer stack. `files` is the exception:
   * uploading takes a stream in the effect type, which a one-way transformation cannot convert.
   */
  def mapK[G[_]](fk: F ~> G): AnthropicClient[G] = {
    val self = this
    new AnthropicClient[G] {
      val messages: Messages[G] = self.messages.mapK(fk)
      val models: Models[G]     = self.models.mapK(fk)
      val batches: Batches[G]   = self.batches.mapK(fk)
      // Uploads consume a stream in the effect type, which a one-way transformation cannot convert.
      def files: Files[G] =
        throw new UnsupportedOperationException(
          "files is not available on a client transformed with mapK; use the original client"
        )
    }
  }
}

object AnthropicClient {

  /**
   * Builds a client on top of any http4s `Client`. Use this to bring your own HTTP backend or middleware; the
   * `anthropic4s` artifact offers `resource`, which sets up Ember for you.
   *
   * The http4s client must not impose a short request timeout of its own: long generations are normal.
   */
  def fromClient[F[_]: Async](
      http: Client[F],
      config: AnthropicConfig,
      telemetry: Telemetry[F]
  ): AnthropicClient[F] = {
    val executor = new Executor[F](http, config)
    new AnthropicClient[F] {
      val messages: Messages[F] = new MessagesImpl[F](executor, config, telemetry)
      val models: Models[F]     = new ModelsImpl[F](executor, telemetry)
      val batches: Batches[F]   = new BatchesImpl[F](executor, telemetry)
      val files: Files[F]       = new FilesImpl[F](executor, telemetry)
    }
  }

  def fromClient[F[_]: Async](http: Client[F], config: AnthropicConfig): AnthropicClient[F] =
    fromClient(http, config, Telemetry.noop[F])
}

/** Lets the tool runner route its requests through its own telemetry scope. */
private[anthropic4s] trait TelemetryAware[F[_]] {
  def telemetry: Telemetry[F]
  def withTelemetry(t: Telemetry[F]): Messages[F]
}

final private[anthropic4s] class MessagesImpl[F[_]](
    executor: Executor[F],
    config: AnthropicConfig,
    val telemetry: Telemetry[F]
)(implicit F: Async[F])
    extends Messages[F]
    with TelemetryAware[F] {

  def withTelemetry(t: Telemetry[F]): Messages[F] = new MessagesImpl[F](executor, config, t)

  /**
   * The official SDKs estimate a generation at one hour per 128k output tokens and refuse non-streaming
   * requests that would outlast the timeout, instead of letting them fail after minutes of waiting.
   */
  private def guardLongRequest(request: MessageRequest, options: RequestOptions): F[Unit] = {
    val expected = (3600.0 * request.maxTokens.toDouble / 128000.0).seconds
    val limit    = config.timeout
    F.raiseWhen(options.timeout.isEmpty && expected > limit)(
      new AnthropicError.StreamingRequired(expected, limit)
    )
  }

  def createRaw(request: MessageRequest, options: RequestOptions): F[ApiResponse[Message]] =
    guardLongRequest(request, options) *>
      executor.json[Message](
        Call[F](
          "messages.create",
          Method.POST,
          "/v1/messages",
          options,
          body = Some(request.asJson),
          message = Some(request)
        ),
        telemetry
      )((scope, message) => scope.completed(message))

  def create(request: MessageRequest, options: RequestOptions): F[Message] =
    createRaw(request, options).map(_.value)

  def stream(request: MessageRequest, options: RequestOptions): Stream[F, StreamEvent] = {
    val body = request.asJson.mapObject(_.add("stream", true.asJson))
    val call = Call[F](
      "messages.create",
      Method.POST,
      "/v1/messages",
      options,
      body = Some(body),
      message = Some(request),
      streaming = true
    )
    val idle = options.timeout.getOrElse(config.timeout)
    executor.open(call, telemetry).flatMap { case (scope, response) =>
      Sse.events(response.body, idle, scope)
    }
  }

  def countTokens(request: MessageRequest, options: RequestOptions): F[TokenCount] =
    executor
      .json[TokenCount](
        Call[F](
          "messages.count_tokens",
          Method.POST,
          "/v1/messages/count_tokens",
          options,
          body = Some(CountTokensRequest.from(request).asJson),
          message = Some(request)
        ),
        telemetry
      )((_, _) => F.unit)
      .map(_.value)
}

final private[anthropic4s] class ModelsImpl[F[_]](executor: Executor[F], telemetry: Telemetry[F])(implicit
    F: Async[F]
) extends Models[F] {

  def retrieve(model: Model): F[ModelInfo] =
    executor
      .json[ModelInfo](
        Call[F]("models.retrieve", Method.GET, s"/v1/models/${model.value}", RequestOptions.default),
        telemetry
      )((_, _) => F.unit)
      .map(_.value)

  def list(params: ListParams): F[Page[ModelInfo]] =
    executor
      .json[Page[ModelInfo]](
        Call[F]("models.list", Method.GET, "/v1/models", RequestOptions.default, query = params.query),
        telemetry
      )((_, _) => F.unit)
      .map(_.value)

  def listAll: Stream[F, ModelInfo] = internal.Paging.all(ListParams.default)(list)
}
