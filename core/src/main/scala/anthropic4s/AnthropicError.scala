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

import anthropic4s.schema.SchemaIssue
import cats.data.NonEmptyList

import scala.concurrent.duration.FiniteDuration

/**
 * Everything this SDK raises. Errors travel in `F` (a failed `IO`, a failed `Future`), so recover with
 * `attemptNarrow[AnthropicError]`, `recoverWith` or `handleErrorWith` and match by type:
 * {{{
 * client.messages.create(request).recoverWith {
 *   case e: AnthropicError.RateLimited => IO.sleep(e.retryAfter.getOrElse(5.seconds)) *> fallback
 *   case e: AnthropicError.Api         => IO.println(e.errorMessage) *> fallback
 * }
 * }}}
 */
sealed abstract class AnthropicError(message: String, cause: Throwable) extends Exception(message, cause)

object AnthropicError {

  /** The API answered with an error status. */
  sealed abstract class Api(
      val status: Int,
      val errorType: String,
      val errorMessage: String,
      val requestId: Option[String],
      val body: String
  ) extends AnthropicError(
        s"$status $errorType: $errorMessage" + requestId.fold("")(id => s" (request id: $id)"),
        null
      )

  // 4xx: the request is at fault; retrying the same request will not help.
  final class BadRequest private[anthropic4s] (t: String, m: String, id: Option[String], b: String)
      extends Api(400, t, m, id, b)
  final class Authentication private[anthropic4s] (t: String, m: String, id: Option[String], b: String)
      extends Api(401, t, m, id, b)
  final class Billing private[anthropic4s] (t: String, m: String, id: Option[String], b: String)
      extends Api(402, t, m, id, b)
  final class PermissionDenied private[anthropic4s] (t: String, m: String, id: Option[String], b: String)
      extends Api(403, t, m, id, b)
  final class NotFound private[anthropic4s] (t: String, m: String, id: Option[String], b: String)
      extends Api(404, t, m, id, b)
  final class Conflict private[anthropic4s] (t: String, m: String, id: Option[String], b: String)
      extends Api(409, t, m, id, b)
  final class RequestTooLarge private[anthropic4s] (t: String, m: String, id: Option[String], b: String)
      extends Api(413, t, m, id, b)
  final class UnprocessableEntity private[anthropic4s] (t: String, m: String, id: Option[String], b: String)
      extends Api(422, t, m, id, b)

  /** 429. `retryAfter` is the server's hint, when it gave one. */
  final class RateLimited private[anthropic4s] (
      t: String,
      m: String,
      id: Option[String],
      b: String,
      val retryAfter: Option[FiniteDuration]
  ) extends Api(429, t, m, id, b)

  /** 529: the API is temporarily overloaded. */
  final class Overloaded private[anthropic4s] (t: String, m: String, id: Option[String], b: String)
      extends Api(529, t, m, id, b)

  /** Any other 5xx. */
  final class Server private[anthropic4s] (s: Int, t: String, m: String, id: Option[String], b: String)
      extends Api(s, t, m, id, b)

  /** A status this SDK has no dedicated type for. */
  final class UnexpectedStatus private[anthropic4s] (
      s: Int,
      t: String,
      m: String,
      id: Option[String],
      b: String
  ) extends Api(s, t, m, id, b)

  /** The request never got an HTTP response (DNS, TLS, connection reset, ...). */
  final class Connection private[anthropic4s] (cause: Throwable)
      extends AnthropicError(s"connection error: ${cause.getMessage}", cause)

  /** No response (or no further stream data) within the allowed time. */
  final class Timeout private[anthropic4s] (val after: FiniteDuration)
      extends AnthropicError(s"request timed out after $after", null)

  /** A response arrived but was not the JSON this SDK expected. */
  final class Decoding private[anthropic4s] (val detail: String, val body: String, cause: Throwable)
      extends AnthropicError(s"could not decode the API response: $detail", cause)

  /** The API reported an error in the middle of a stream. */
  final class StreamFailure private[anthropic4s] (val errorType: String, val errorMessage: String)
      extends AnthropicError(s"stream failed with $errorType: $errorMessage", null)

  /** The stream ended before the message was complete. */
  final class StreamTruncated private[anthropic4s] ()
      extends AnthropicError("the stream ended before message_stop", null)

  /**
   * A non-streaming request with this `max_tokens` may run longer than the timeout allows. Use
   * `messages.stream(...).finalMessage`, or set an explicit timeout in `RequestOptions`.
   */
  final class StreamingRequired private[anthropic4s] (val expected: FiniteDuration, val limit: FiniteDuration)
      extends AnthropicError(
        s"this request may take about $expected, longer than the $limit non-streaming limit; " +
          "stream it (messages.stream(...).finalMessage) or set RequestOptions.withTimeout explicitly",
        null
      )

  /**
   * The type given to `parse` or a tool cannot be expressed as a strict JSON Schema. Raised before any
   * request.
   */
  final class InvalidSchema private[anthropic4s] (val issues: NonEmptyList[SchemaIssue])
      extends AnthropicError("invalid schema: " + issues.map(_.message).toList.mkString("; "), null)

  /**
   * A structured-output reply could not be turned into the requested type: the model refused, the reply was
   * cut off, or the JSON did not match. `reply` is the raw message.
   */
  final class OutputParse private[anthropic4s] (val detail: String, val reply: Message)
      extends AnthropicError(s"could not read the structured output: $detail", null)

  /** The client was set up incorrectly (missing credentials, malformed base URL, ...). */
  final class Configuration private[anthropic4s] (val detail: String) extends AnthropicError(detail, null)

  private[anthropic4s] def fromStatus(
      status: Int,
      errorType: String,
      message: String,
      requestId: Option[String],
      body: String,
      retryAfter: Option[FiniteDuration]
  ): Api = status match {
    case 400           => new BadRequest(errorType, message, requestId, body)
    case 401           => new Authentication(errorType, message, requestId, body)
    case 402           => new Billing(errorType, message, requestId, body)
    case 403           => new PermissionDenied(errorType, message, requestId, body)
    case 404           => new NotFound(errorType, message, requestId, body)
    case 409           => new Conflict(errorType, message, requestId, body)
    case 413           => new RequestTooLarge(errorType, message, requestId, body)
    case 422           => new UnprocessableEntity(errorType, message, requestId, body)
    case 429           => new RateLimited(errorType, message, requestId, body, retryAfter)
    case 529           => new Overloaded(errorType, message, requestId, body)
    case s if s >= 500 => new Server(s, errorType, message, requestId, body)
    case s             => new UnexpectedStatus(s, errorType, message, requestId, body)
  }
}
