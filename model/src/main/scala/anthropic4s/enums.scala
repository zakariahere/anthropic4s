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

import cats.{Eq, Show}
import io.circe.{Codec, Decoder, Encoder}

// Open enums.
//
// The API adds values all the time, so none of these are sealed hierarchies: each is a thin wrapper around
// the wire string with named constants. Match on the constants (`case StopReason.EndTurn =>`) and keep a
// default branch for values this SDK version does not know yet.

private[anthropic4s] object OpenEnum {
  def codec[A](make: String => A)(value: A => String): Codec[A] =
    Codec.from(Decoder[String].map(make), Encoder[String].contramap(value))
}

/** A model id, e.g. `Model.ClaudeOpus5_5` or `Model.of("claude-...")` for one released later. */
final case class Model private (value: String) {
  override def toString: String = value
}
object Model {
  def of(value: String): Model = new Model(value)

  val ClaudeFable5_2: Model  = of("claude-fable-5-2")
  val ClaudeOpus5_5: Model   = of("claude-opus-5-5")
  val ClaudeOpus5: Model     = of("claude-opus-5")
  val ClaudeOpus4_8: Model   = of("claude-opus-4-8")
  val ClaudeSonnet5_5: Model = of("claude-sonnet-5-5")
  val ClaudeSonnet5: Model   = of("claude-sonnet-5")
  val ClaudeHaiku5_5: Model  = of("claude-haiku-5-5")
  val ClaudeHaiku4_5: Model  = of("claude-haiku-4-5")

  implicit val codec: Codec[Model] = OpenEnum.codec(of)(_.value)
  implicit val eq: Eq[Model]       = Eq.fromUniversalEquals
  implicit val show: Show[Model]   = Show.show(_.value)
}

/** Why the model stopped generating. */
final case class StopReason private (value: String) {
  override def toString: String = value
}
object StopReason {
  def of(value: String): StopReason = new StopReason(value)

  val EndTurn: StopReason      = of("end_turn")
  val MaxTokens: StopReason    = of("max_tokens")
  val StopSequence: StopReason = of("stop_sequence")
  val ToolUse: StopReason      = of("tool_use")
  val PauseTurn: StopReason    = of("pause_turn")
  val Refusal: StopReason      = of("refusal")

  implicit val codec: Codec[StopReason] = OpenEnum.codec(of)(_.value)
  implicit val eq: Eq[StopReason]       = Eq.fromUniversalEquals
  implicit val show: Show[StopReason]   = Show.show(_.value)
}

/** Who wrote a message. `System` is only valid for mid-conversation operator instructions. */
final case class Role private (value: String) {
  override def toString: String = value
}
object Role {
  def of(value: String): Role = new Role(value)

  val User: Role      = of("user")
  val Assistant: Role = of("assistant")
  val System: Role    = of("system")

  implicit val codec: Codec[Role] = OpenEnum.codec(of)(_.value)
  implicit val eq: Eq[Role]       = Eq.fromUniversalEquals
  implicit val show: Show[Role]   = Show.show(_.value)
}

/** How hard the model works on a request: thinking depth and overall token spend. */
final case class Effort private (value: String) {
  override def toString: String = value
}
object Effort {
  def of(value: String): Effort = new Effort(value)

  val Low: Effort    = of("low")
  val Medium: Effort = of("medium")
  val High: Effort   = of("high")
  val XHigh: Effort  = of("xhigh")
  val Max: Effort    = of("max")

  implicit val codec: Codec[Effort] = OpenEnum.codec(of)(_.value)
  implicit val eq: Eq[Effort]       = Eq.fromUniversalEquals
  implicit val show: Show[Effort]   = Show.show(_.value)
}

/** What a `thinking` block shows; the reasoning itself happens and is billed either way. */
final case class ThinkingDisplay private (value: String) {
  override def toString: String = value
}
object ThinkingDisplay {
  def of(value: String): ThinkingDisplay = new ThinkingDisplay(value)

  val Summarized: ThinkingDisplay = of("summarized")
  val Omitted: ThinkingDisplay    = of("omitted")
  val Updates: ThinkingDisplay    = of("updates")

  implicit val codec: Codec[ThinkingDisplay] = OpenEnum.codec(of)(_.value)
  implicit val eq: Eq[ThinkingDisplay]       = Eq.fromUniversalEquals
}

/** How long a cache breakpoint lives. */
final case class CacheTtl private (value: String) {
  override def toString: String = value
}
object CacheTtl {
  def of(value: String): CacheTtl = new CacheTtl(value)

  val FiveMinutes: CacheTtl = of("5m")
  val OneHour: CacheTtl     = of("1h")

  implicit val codec: Codec[CacheTtl] = OpenEnum.codec(of)(_.value)
  implicit val eq: Eq[CacheTtl]       = Eq.fromUniversalEquals
}

/** A beta feature flag, sent in the `anthropic-beta` header. */
final case class Beta private (value: String) {
  override def toString: String = value
}
object Beta {
  def of(value: String): Beta = new Beta(value)

  val ServerSideFallback: Beta = of("server-side-fallback-2026-07-01")
  val Compaction: Beta         = of("compact-2026-01-12")
  val ContextManagement: Beta  = of("context-management-2025-06-27")
  val TaskBudgets: Beta        = of("task-budgets-2026-03-13")
  val McpClient: Beta          = of("mcp-client-2025-11-20")
  val MessageThreads: Beta     = of("message-threads-2026-08-12")
  val CacheDiagnosis: Beta     = of("cache-diagnosis-2026-04-07")

  implicit val codec: Codec[Beta] = OpenEnum.codec(of)(_.value)
  implicit val eq: Eq[Beta]       = Eq.fromUniversalEquals
  implicit val show: Show[Beta]   = Show.show(_.value)
}

/** Processing state of a message batch. */
final case class BatchStatus private (value: String) {
  override def toString: String = value
}
object BatchStatus {
  def of(value: String): BatchStatus = new BatchStatus(value)

  val InProgress: BatchStatus = of("in_progress")
  val Canceling: BatchStatus  = of("canceling")
  val Ended: BatchStatus      = of("ended")

  implicit val codec: Codec[BatchStatus] = OpenEnum.codec(of)(_.value)
  implicit val eq: Eq[BatchStatus]       = Eq.fromUniversalEquals
}
