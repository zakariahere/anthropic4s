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

import anthropic4s.internal.Wire
import cats.{Eq, Monoid, Show}
import io.circe.{Decoder, Encoder, JsonObject}

/**
 * Token accounting for one response.
 *
 * `Usage` forms a [[cats.Monoid]]: counters add up, so the usage of a whole agent run is
 * `turns.foldMap(_.usage)`.
 */
final case class Usage private (
    inputTokens: Long,
    outputTokens: Long,
    cacheCreationInputTokens: Long,
    cacheReadInputTokens: Long,
    serviceTier: Option[String],
    extras: JsonObject
) {

  /** Everything the model read: fresh input plus cache writes and cache reads. */
  def totalInputTokens: Long = inputTokens + cacheCreationInputTokens + cacheReadInputTokens

  def withInputTokens(n: Long): Usage              = copy(inputTokens = n)
  def withOutputTokens(n: Long): Usage             = copy(outputTokens = n)
  def withCacheCreationInputTokens(n: Long): Usage = copy(cacheCreationInputTokens = n)
  def withCacheReadInputTokens(n: Long): Usage     = copy(cacheReadInputTokens = n)
  def withServiceTier(tier: String): Usage         = copy(serviceTier = Some(tier))
}

object Usage {

  val empty: Usage = new Usage(0L, 0L, 0L, 0L, None, JsonObject.empty)

  def apply(inputTokens: Long, outputTokens: Long): Usage =
    new Usage(inputTokens, outputTokens, 0L, 0L, None, JsonObject.empty)

  // Hides the synthetic extractor: positional patterns would break every time a field is added.
  @scala.annotation.unused
  private def unapply(u: Usage): Some[Usage] = Some(u)

  implicit val monoid: Monoid[Usage] = new Monoid[Usage] {
    val empty: Usage                       = Usage.empty
    def combine(x: Usage, y: Usage): Usage =
      new Usage(
        x.inputTokens + y.inputTokens,
        x.outputTokens + y.outputTokens,
        x.cacheCreationInputTokens + y.cacheCreationInputTokens,
        x.cacheReadInputTokens + y.cacheReadInputTokens,
        y.serviceTier.orElse(x.serviceTier),
        // Nested counters we do not model cannot be summed safely, so the latest ones win.
        if (y.extras.isEmpty) x.extras else y.extras
      )
  }

  implicit val eq: Eq[Usage] = Eq.fromUniversalEquals

  implicit val show: Show[Usage] = Show.show { u =>
    s"Usage(in=${u.inputTokens}, out=${u.outputTokens}, cacheWrite=${u.cacheCreationInputTokens}, " +
      s"cacheRead=${u.cacheReadInputTokens})"
  }

  implicit val decoder: Decoder[Usage] = Wire.decoder { r =>
    for {
      in    <- r.optional[Long]("input_tokens")
      out   <- r.optional[Long]("output_tokens")
      write <- r.optional[Long]("cache_creation_input_tokens")
      read  <- r.optional[Long]("cache_read_input_tokens")
      tier  <- r.optional[String]("service_tier")
    } yield new Usage(
      in.getOrElse(0L),
      out.getOrElse(0L),
      write.getOrElse(0L),
      read.getOrElse(0L),
      tier,
      r.extras
    )
  }

  implicit val encoder: Encoder[Usage] = Encoder.instance { u =>
    Wire.writer
      .field("input_tokens", u.inputTokens)
      .field("output_tokens", u.outputTokens)
      .field("cache_creation_input_tokens", u.cacheCreationInputTokens)
      .field("cache_read_input_tokens", u.cacheReadInputTokens)
      .optional("service_tier", u.serviceTier)
      .toJson(u.extras)
  }
}
