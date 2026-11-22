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

import scala.concurrent.duration._
import scala.util.Try

/** What one HTTP attempt ended with, as far as retrying is concerned. */
sealed trait AttemptOutcome extends Product with Serializable
object AttemptOutcome {

  /** A response arrived. `header` looks a response header up by (case-insensitive) name. */
  final case class Status(code: Int, header: String => Option[String]) extends AttemptOutcome
  case object ConnectionFailure                                        extends AttemptOutcome
  case object TimedOut                                                 extends AttemptOutcome
}

sealed trait RetryDecision extends Product with Serializable
object RetryDecision {
  case object GiveUp                                 extends RetryDecision
  final case class RetryAfter(delay: FiniteDuration) extends RetryDecision
}

/**
 * When and how long to wait before sending a request again.
 *
 * The default matches the official SDKs: up to 2 retries on connection errors, timeouts, 408, 409, 429 and
 * 5xx, with exponential backoff and jitter. The server's `x-should-retry` and `retry-after` headers take
 * precedence over the defaults.
 */
final case class RetryPolicy private (
    maxRetries: Int,
    initialDelay: FiniteDuration,
    maxDelay: FiniteDuration,
    jitter: Double
) {
  def withMaxRetries(n: Int): RetryPolicy              = copy(maxRetries = math.max(0, n))
  def withInitialDelay(d: FiniteDuration): RetryPolicy = copy(initialDelay = d)
  def withMaxDelay(d: FiniteDuration): RetryPolicy     = copy(maxDelay = d)

  /**
   * Pure decision function, so the policy can be unit-tested without a clock.
   *
   * @param retriesSoFar
   *   how many retries were already made (0 after the first attempt)
   * @param rand
   *   a random number in [0, 1), used for jitter
   */
  def decide(retriesSoFar: Int, outcome: AttemptOutcome, rand: Double): RetryDecision =
    if (retriesSoFar >= maxRetries || !RetryPolicy.shouldRetry(outcome)) RetryDecision.GiveUp
    else RetryDecision.RetryAfter(RetryPolicy.serverDelay(outcome).getOrElse(backoff(retriesSoFar, rand)))

  private def backoff(retriesSoFar: Int, rand: Double): FiniteDuration = {
    val exponential = initialDelay.toMillis.toDouble * math.pow(2.0, retriesSoFar.toDouble)
    val capped      = math.min(exponential, maxDelay.toMillis.toDouble)
    (capped * (1.0 - jitter * rand)).toLong.millis
  }
}

object RetryPolicy {

  val default: RetryPolicy = new RetryPolicy(2, 500.millis, 8.seconds, 0.25)

  /** Never retry. */
  val none: RetryPolicy = default.withMaxRetries(0)

  @scala.annotation.unused
  private def unapply(p: RetryPolicy): Some[RetryPolicy] = Some(p)

  private def shouldRetry(outcome: AttemptOutcome): Boolean = outcome match {
    case AttemptOutcome.ConnectionFailure | AttemptOutcome.TimedOut => true
    case AttemptOutcome.Status(code, header)                        =>
      header("x-should-retry") match {
        case Some("true")  => true
        case Some("false") => false
        case _             => code == 408 || code == 409 || code == 429 || code >= 500
      }
  }

  /** `retry-after-ms` or `retry-after` (seconds), when present and no longer than a minute. */
  private def serverDelay(outcome: AttemptOutcome): Option[FiniteDuration] = outcome match {
    case AttemptOutcome.Status(_, header) =>
      val millis  = header("retry-after-ms").flatMap(v => Try(v.trim.toDouble).toOption)
      val seconds = header("retry-after").flatMap(v => Try(v.trim.toDouble).toOption).map(_ * 1000.0)
      millis.orElse(seconds).filter(ms => ms > 0 && ms <= 60000.0).map(_.toLong.millis)
    case _ => None
  }

  private[anthropic4s] def retryAfterHint(header: String => Option[String]): Option[FiniteDuration] =
    serverDelay(AttemptOutcome.Status(429, header))
}
