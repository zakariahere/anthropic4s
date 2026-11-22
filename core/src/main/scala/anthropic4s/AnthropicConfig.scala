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
import cats.{MonadThrow, Show}
import cats.effect.std.Env

import scala.concurrent.duration._

/** Wraps a sensitive value so it never ends up in logs: `toString` and `Show` print `***`. */
final class Secret[A] private (val value: A) {
  override def toString: String        = "Secret(***)"
  override def hashCode: Int           = value.hashCode
  override def equals(o: Any): Boolean = o match {
    case s: Secret[_] => s.value == value
    case _            => false
  }
}
object Secret {
  def apply[A](value: A): Secret[A]     = new Secret(value)
  implicit def show[A]: Show[Secret[A]] = Show.show(_ => "***")
}

/** How the client authenticates. */
sealed trait Credentials extends Product with Serializable
object Credentials {

  /** An API key, sent as `x-api-key`. */
  final case class ApiKey(key: Secret[String]) extends Credentials

  /** An OAuth access token, sent as `Authorization: Bearer`. */
  final case class AuthToken(token: Secret[String]) extends Credentials
}

/**
 * Client settings. Start from an API key or the environment and adjust with `withX`:
 * {{{
 * AnthropicConfig.fromEnv[IO].map(_.withTimeout(2.minutes).withBetas(Beta.ServerSideFallback))
 * }}}
 */
final case class AnthropicConfig private (
    credentials: Credentials,
    baseUrl: String,
    apiVersion: String,
    timeout: FiniteDuration,
    retryPolicy: RetryPolicy,
    betas: Set[Beta],
    headers: List[(String, String)]
) {
  def withCredentials(c: Credentials): AnthropicConfig = copy(credentials = c)
  def withBaseUrl(url: String): AnthropicConfig        = copy(baseUrl = url)
  def withApiVersion(v: String): AnthropicConfig       = copy(apiVersion = v)
  def withRetryPolicy(p: RetryPolicy): AnthropicConfig = copy(retryPolicy = p)
  def withMaxRetries(n: Int): AnthropicConfig          = copy(retryPolicy = retryPolicy.withMaxRetries(n))

  /**
   * Time allowed for one attempt of a non-streaming request, and the longest silence tolerated on a stream.
   * Retries each get the full amount.
   */
  def withTimeout(d: FiniteDuration): AnthropicConfig = copy(timeout = d)

  /** Beta flags sent with every request. */
  def withBetas(bs: Beta*): AnthropicConfig = copy(betas = betas ++ bs)

  /** An extra header sent with every request. */
  def withHeader(name: String, value: String): AnthropicConfig = copy(headers = headers :+ (name -> value))
}

object AnthropicConfig {

  val DefaultBaseUrl: String         = "https://api.anthropic.com"
  val DefaultApiVersion: String      = "2026-10-21"
  val DefaultTimeout: FiniteDuration = 10.minutes

  def apply(credentials: Credentials): AnthropicConfig =
    new AnthropicConfig(
      credentials,
      DefaultBaseUrl,
      DefaultApiVersion,
      DefaultTimeout,
      RetryPolicy.default,
      Set.empty,
      Nil
    )

  def apply(apiKey: String): AnthropicConfig = apply(Credentials.ApiKey(Secret(apiKey)))

  @scala.annotation.unused
  private def unapply(c: AnthropicConfig): Some[AnthropicConfig] = Some(c)

  /**
   * Reads `ANTHROPIC_API_KEY` (or, failing that, `ANTHROPIC_AUTH_TOKEN`) and the optional
   * `ANTHROPIC_BASE_URL`. Fails with [[AnthropicError.Configuration]] when no credential is set.
   */
  def fromEnv[F[_]](implicit F: MonadThrow[F], env: Env[F]): F[AnthropicConfig] =
    (env.get("ANTHROPIC_API_KEY"), env.get("ANTHROPIC_AUTH_TOKEN"), env.get("ANTHROPIC_BASE_URL")).tupled
      .flatMap { case (apiKey, token, baseUrl) =>
        val credentials: Option[Credentials] =
          apiKey
            .filter(_.trim.nonEmpty)
            .map(k => Credentials.ApiKey(Secret(k)))
            .orElse(token.filter(_.trim.nonEmpty).map(t => Credentials.AuthToken(Secret(t))))
        credentials match {
          case Some(c) =>
            val base = apply(c)
            F.pure(baseUrl.filter(_.trim.nonEmpty).fold(base)(base.withBaseUrl))
          case None =>
            F.raiseError(
              new AnthropicError.Configuration(
                "no credentials: set ANTHROPIC_API_KEY or ANTHROPIC_AUTH_TOKEN"
              )
            )
        }
      }
}

/**
 * Per-request settings; anything left unset falls back to the client's [[AnthropicConfig]].
 * {{{
 * RequestOptions.default.withTimeout(30.seconds).withBetas(Beta.TaskBudgets)
 * }}}
 */
final case class RequestOptions private (
    timeout: Option[FiniteDuration],
    retryPolicy: Option[RetryPolicy],
    betas: Set[Beta],
    headers: List[(String, String)],
    query: List[(String, String)],
    conversationId: Option[String]
) {
  def withTimeout(d: FiniteDuration): RequestOptions  = copy(timeout = Some(d))
  def withRetryPolicy(p: RetryPolicy): RequestOptions = copy(retryPolicy = Some(p))
  def withMaxRetries(n: Int): RequestOptions          =
    copy(retryPolicy = Some(retryPolicy.getOrElse(RetryPolicy.default).withMaxRetries(n)))
  def withBetas(bs: Beta*): RequestOptions                        = copy(betas = betas ++ bs)
  def withHeader(name: String, value: String): RequestOptions     = copy(headers = headers :+ (name -> value))
  def withQueryParam(name: String, value: String): RequestOptions = copy(query = query :+ (name -> value))

  /** Groups the traces of one conversation (`gen_ai.conversation.id`). Only used by telemetry. */
  def withConversationId(id: String): RequestOptions = copy(conversationId = Some(id))
}

object RequestOptions {
  val default: RequestOptions = new RequestOptions(None, None, Set.empty, Nil, Nil, None)

  @scala.annotation.unused
  private def unapply(o: RequestOptions): Some[RequestOptions] = Some(o)
}

/** A decoded response together with the HTTP facts around it. */
final case class ApiResponse[A] private[anthropic4s] (
    value: A,
    status: Int,
    headers: List[(String, String)],
    requestId: Option[String]
) {

  /** A response header by case-insensitive name, e.g. `anthropic-ratelimit-requests-remaining`. */
  def header(name: String): Option[String] = headers.collectFirst {
    case (k, v) if k.equalsIgnoreCase(name) => v
  }
}
