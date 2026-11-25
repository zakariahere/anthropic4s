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

import org.http4s.Uri

import scala.concurrent.duration._
import scala.util.control.NoStackTrace

/** Where the decision model is served, and how to reach it. */
final case class DecideConfig(endpoint: Uri, apiKey: Option[String], timeout: FiniteDuration) {
  def withEndpoint(uri: Uri): DecideConfig         = copy(endpoint = uri)
  def withApiKey(key: String): DecideConfig        = copy(apiKey = Some(key))
  def withTimeout(d: FiniteDuration): DecideConfig = copy(timeout = d)

  // Keeps the key out of logs.
  override def toString: String =
    s"DecideConfig($endpoint, apiKey = ${apiKey.fold("none")(_ => "****")}, $timeout)"
}

object DecideConfig {

  /**
   * Generous enough for a cold first request, while llama-server warms up (about 2.5 s). Raise it if your
   * local setup is slower.
   */
  val DefaultTimeout: FiniteDuration = 10.seconds

  /** A System One server at this base URL */
  def apply(baseUrl: Uri): DecideConfig = DecideConfig(baseUrl.addPath("v1/systemone"), None, DefaultTimeout)

  def local(port: Int = 8081): DecideConfig = apply(Uri.unsafeFromString(s"http://localhost:$port"))

}

sealed abstract class DecideError(message: String) extends RuntimeException(message) with NoStackTrace

object DecideError {
  final class InvalidQuestion(val detail: String) extends DecideError(s"Invalid question: $detail")
  final class Http(val status: Int, val body: String)
      extends DecideError(s"decision endpoint answered $status: $body")
  final class Decode(val detail: String)         extends DecideError(s"unexpected answer: $detail")
  final class Timeout(val after: FiniteDuration) extends DecideError(s"no answer within $after")
}
