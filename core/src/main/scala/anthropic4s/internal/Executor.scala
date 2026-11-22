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

package anthropic4s.internal

import anthropic4s._
import cats.effect.syntax.all._
import cats.effect.{Async, Ref, Resource}
import cats.syntax.all._
import fs2.Stream
import io.circe.{Decoder, Json}
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.{Header, Method, Request, Response, Uri}
import org.typelevel.ci.CIString

import scala.concurrent.duration.FiniteDuration

/** A failed attempt: the error to raise if we give up, and what the retry policy needs to decide. */
final private[internal] case class Failure(error: AnthropicError, outcome: AttemptOutcome)

/** One API call, described as data. */
final private[anthropic4s] case class Call[F[_]](
    operation: String,
    method: Method,
    route: String,
    options: RequestOptions,
    query: List[(String, String)] = Nil,
    body: Option[Json] = None,
    message: Option[MessageRequest] = None,
    streaming: Boolean = false,
    customize: Request[F] => Request[F] = (r: Request[F]) => r
)

/**
 * Sends [[Call]]s: builds the HTTP request, applies the timeout, retries according to the policy, maps error
 * statuses to [[AnthropicError]]s and reports everything to [[Telemetry]].
 *
 * A request is retried only until response headers arrive. Once a body has started (a stream above all)
 * nothing is sent again, because the caller may already have acted on what it received.
 */
final private[anthropic4s] class Executor[F[_]](http: Client[F], config: AnthropicConfig)(implicit
    F: Async[F]
) {

  private val baseUri: Either[AnthropicError, Uri] =
    Uri
      .fromString(config.baseUrl)
      .leftMap(e => new AnthropicError.Configuration(s"invalid base URL: ${e.sanitized}"))

  private val userAgent: String =
    "anthropic4s" + Option(getClass.getPackage)
      .flatMap(p => Option(p.getImplementationVersion))
      .fold("")("/" + _)

  private def timeoutFor(call: Call[F]): FiniteDuration = call.options.timeout.getOrElse(config.timeout)
  private def policyFor(call: Call[F]): RetryPolicy = call.options.retryPolicy.getOrElse(config.retryPolicy)
  private val random: F[Double] = F.delay(java.util.concurrent.ThreadLocalRandom.current().nextDouble())

  private def uriFor(call: Call[F]): Either[AnthropicError, Uri] =
    baseUri.map { base =>
      val path = Uri.Path.unsafeFromString(base.path.renderString.stripSuffix("/") + call.route)
      (call.query ++ call.options.query).foldLeft(base.withPath(path)) { case (u, (k, v)) =>
        u.withQueryParam(k, v)
      }
    }

  private def context(call: Call[F], uri: Uri): RequestContext =
    RequestContext(
      call.operation,
      call.method.name,
      call.route,
      uri.renderString,
      call.message,
      call.streaming,
      call.options
    )

  private def build(call: Call[F], uri: Uri, propagation: List[(String, String)]): Request[F] = {
    val auth = config.credentials match {
      case Credentials.ApiKey(key)      => "x-api-key"     -> key.value
      case Credentials.AuthToken(token) => "authorization" -> s"Bearer ${token.value}"
    }
    val betas   = (config.betas ++ call.options.betas).map(_.value).toList.sorted
    val headers =
      List(auth, "anthropic-version" -> config.apiVersion, "user-agent" -> userAgent) ++
        (if (betas.isEmpty) Nil else List("anthropic-beta" -> betas.mkString(","))) ++
        propagation ++ config.headers ++ call.options.headers

    val base     = Request[F](call.method, uri)
    val withBody = call.body.fold(base)(json => base.withEntity(json))
    // Later entries win, so per-request headers override client-wide ones.
    call.customize(headers.foldLeft(withBody) { case (r, (k, v)) =>
      r.putHeaders(Header.Raw(CIString(k), v))
    })
  }

  private def headerLookup(resp: Response[F]): String => Option[String] =
    name => resp.headers.get(CIString(name)).map(_.head.value)

  private def meta(resp: Response[F]): ResponseMeta =
    ResponseMeta(
      resp.status.code,
      headerLookup(resp)("request-id"),
      resp.headers.headers.map(h => h.name.toString -> h.value)
    )

  private def failureOf(resp: Response[F], body: String): Failure = {
    val lookup  = headerLookup(resp)
    val decoded = io.circe.parser.decode[ApiErrorBody](body).toOption
    val error   = AnthropicError.fromStatus(
      resp.status.code,
      decoded.fold("unknown_error")(_.errorType),
      decoded.map(_.message).filter(_.nonEmpty).getOrElse(body.take(500)),
      lookup("request-id").orElse(decoded.flatMap(_.requestId)),
      body,
      RetryPolicy.retryAfterHint(lookup)
    )
    Failure(error, AttemptOutcome.Status(resp.status.code, lookup))
  }

  private def connectionFailure(e: Throwable): Failure =
    Failure(new AnthropicError.Connection(e), AttemptOutcome.ConnectionFailure)

  private def timeoutFailure(after: FiniteDuration): Failure =
    Failure(new AnthropicError.Timeout(after), AttemptOutcome.TimedOut)

  /** Sleeps and retries, or raises the failure, as the policy says. */
  private def retryOrRaise[A](call: Call[F], retries: Int, failure: Failure)(again: => F[A]): F[A] =
    random.flatMap { rand =>
      policyFor(call).decide(retries, failure.outcome, rand) match {
        case RetryDecision.RetryAfter(delay) => F.sleep(delay) *> again
        case RetryDecision.GiveUp            => F.raiseError(failure.error)
      }
    }

  private def decodeBody[A: Decoder](body: String): F[A] =
    io.circe.parser.decode[A](body) match {
      case Right(a) => F.pure(a)
      case Left(e)  => F.raiseError(new AnthropicError.Decoding(e.getMessage, body, e))
    }

  /** Sends a request and decodes the JSON response. `onSuccess` runs while the telemetry scope is open. */
  def json[A: Decoder](call: Call[F], telemetry: Telemetry[F])(
      onSuccess: (RequestScope[F], A) => F[Unit]
  ): F[ApiResponse[A]] =
    F.fromEither(uriFor(call)).flatMap { uri =>
      telemetry.request(context(call, uri)).use { scope =>
        val timeout = timeoutFor(call)

        def once(request: Request[F]): F[Either[Failure, ApiResponse[A]]] =
          http
            .run(request)
            .use { resp =>
              val m = meta(resp)
              scope.response(m) *> resp.bodyText.compile.string.flatMap { body =>
                if (resp.status.isSuccess)
                  decodeBody[A](body)
                    .map(a => ApiResponse(a, m.status, m.headers, m.requestId).asRight[Failure])
                else F.pure(failureOf(resp, body).asLeft[ApiResponse[A]])
              }
            }
            .timeoutTo(timeout, F.pure(timeoutFailure(timeout).asLeft[ApiResponse[A]]))
            .recoverWith { case e if !e.isInstanceOf[AnthropicError] => F.pure(connectionFailure(e).asLeft) }

        def loop(retries: Int): F[ApiResponse[A]] =
          for {
            _           <- scope.attempt(retries + 1)
            propagation <- scope.headers
            result      <- scope.scope(once(build(call, uri, propagation)))
            response    <- result match {
              case Right(ok)     => onSuccess(scope, ok.value).as(ok)
              case Left(failure) => retryOrRaise(call, retries, failure)(loop(retries + 1))
            }
          } yield response

        loop(0)
      }
    }

  /**
   * Sends a request and hands back the open response for the caller to stream. The connection and the
   * telemetry scope are released when the returned stream finishes, fails or is cancelled.
   */
  def open(call: Call[F], telemetry: Telemetry[F]): Stream[F, (RequestScope[F], Response[F])] =
    Stream.eval(F.fromEither(uriFor(call))).flatMap { uri =>
      Stream.resource(telemetry.request(context(call, uri))).flatMap { scope =>
        Stream.resource(head(call, uri, scope)).map(resp => (scope, resp))
      }
    }

  private def head(call: Call[F], uri: Uri, scope: RequestScope[F]): Resource[F, Response[F]] = {
    val timeout = timeoutFor(call)

    // The finalizer of whichever response is currently open lives in `release`. Storing it atomically
    // with the allocation means a timeout or a cancellation at any point still closes the connection.
    def once(request: Request[F], release: Ref[F, F[Unit]]): F[Either[Failure, Response[F]]] =
      F.uncancelable { poll =>
        poll(http.run(request).allocated).flatMap { case (resp, close) => release.set(close).as(resp) }
      }.flatMap { resp =>
        scope.response(meta(resp)) *> {
          if (resp.status.isSuccess) F.pure(resp.asRight[Failure])
          else
            resp.bodyText.compile.string
              .map(body => failureOf(resp, body).asLeft[Response[F]])
              .guarantee(release.getAndSet(F.unit).flatten)
        }
      }.timeoutTo(timeout, release.getAndSet(F.unit).flatten.as(timeoutFailure(timeout).asLeft[Response[F]]))
        .recoverWith { case e if !e.isInstanceOf[AnthropicError] => F.pure(connectionFailure(e).asLeft) }

    def loop(retries: Int, release: Ref[F, F[Unit]]): F[Response[F]] =
      for {
        _           <- scope.attempt(retries + 1)
        propagation <- scope.headers
        result      <- scope.scope(once(build(call, uri, propagation), release))
        response    <- result match {
          case Right(resp)   => F.pure(resp)
          case Left(failure) => retryOrRaise(call, retries, failure)(loop(retries + 1, release))
        }
      } yield response

    Resource.make(Ref.of[F, F[Unit]](F.unit))(_.get.flatten).evalMap(loop(0, _))
  }
}
