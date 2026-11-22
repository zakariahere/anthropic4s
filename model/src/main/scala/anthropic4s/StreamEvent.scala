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

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}

/** An incremental change to one content block while a response streams. */
sealed trait Delta extends Product with Serializable
object Delta {
  final case class Text(text: String)             extends Delta
  final case class InputJson(partialJson: String) extends Delta
  final case class Thinking(thinking: String)     extends Delta
  final case class Signature(signature: String)   extends Delta
  final case class Citation(citation: Json)       extends Delta

  /** A delta type this SDK version does not know. */
  final case class Unknown(tpe: String, raw: JsonObject) extends Delta

  implicit val decoder: Decoder[Delta] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "text_delta"       => c.get[String]("text").map(Text(_))
      case "input_json_delta" => c.get[String]("partial_json").map(InputJson(_))
      case "thinking_delta"   => c.get[String]("thinking").map(Thinking(_))
      case "signature_delta"  => c.get[String]("signature").map(Signature(_))
      case "citations_delta"  => c.get[Json]("citation").map(Citation(_))
      case other              => c.as[JsonObject].map(Unknown(other, _))
    }
  }

  implicit val encoder: Encoder[Delta] = Encoder.instance {
    case Text(t)         => Json.obj("type" -> "text_delta".asJson, "text" -> t.asJson)
    case InputJson(p)    => Json.obj("type" -> "input_json_delta".asJson, "partial_json" -> p.asJson)
    case Thinking(t)     => Json.obj("type" -> "thinking_delta".asJson, "thinking" -> t.asJson)
    case Signature(s)    => Json.obj("type" -> "signature_delta".asJson, "signature" -> s.asJson)
    case Citation(c)     => Json.obj("type" -> "citations_delta".asJson, "citation" -> c)
    case Unknown(_, raw) => Json.fromJsonObject(raw)
  }
}

/**
 * One server-sent event of a streaming response.
 *
 * `ping` and `error` events never appear here: the client drops pings and turns an `error` event into a
 * failed stream. Fold events with [[MessageAccumulator]] to rebuild the final [[Message]].
 */
sealed trait StreamEvent extends Product with Serializable
object StreamEvent {

  /** The reply has begun. `message` has no content yet and its usage holds only input counts. */
  final case class MessageStart(message: Message) extends StreamEvent

  final case class ContentBlockStart(index: Int, contentBlock: ContentBlock) extends StreamEvent
  final case class ContentBlockDelta(index: Int, delta: Delta)               extends StreamEvent
  final case class ContentBlockStop(index: Int)                              extends StreamEvent

  /** End-of-message facts. `usage` holds cumulative counters as sent by the API. */
  final case class MessageDelta(
      stopReason: Option[StopReason],
      stopSequence: Option[String],
      stopDetails: Option[StopDetails],
      usage: JsonObject
  ) extends StreamEvent

  case object MessageStop extends StreamEvent

  /** An event type this SDK version does not know. */
  final case class Unknown(tpe: String, raw: JsonObject) extends StreamEvent

  implicit val decoder: Decoder[StreamEvent] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "message_start"       => c.get[Message]("message").map(MessageStart(_))
      case "content_block_start" =>
        for { i <- c.get[Int]("index"); b <- c.get[ContentBlock]("content_block") } yield ContentBlockStart(
          i,
          b
        )
      case "content_block_delta" =>
        for { i <- c.get[Int]("index"); d <- c.get[Delta]("delta") } yield ContentBlockDelta(i, d)
      case "content_block_stop" => c.get[Int]("index").map(ContentBlockStop(_))
      case "message_delta"      =>
        val d = c.downField("delta")
        for {
          reason  <- d.get[Option[StopReason]]("stop_reason")
          seq     <- d.get[Option[String]]("stop_sequence")
          details <- d.get[Option[StopDetails]]("stop_details")
          usage   <- c.get[Option[JsonObject]]("usage")
        } yield MessageDelta(reason, seq, details, usage.getOrElse(JsonObject.empty))
      case "message_stop" => Right(MessageStop)
      case other          => c.as[JsonObject].map(Unknown(other, _))
    }
  }

  implicit val encoder: Encoder[StreamEvent] = Encoder.instance {
    case MessageStart(m)         => Json.obj("type" -> "message_start".asJson, "message" -> m.asJson)
    case ContentBlockStart(i, b) =>
      Json.obj("type" -> "content_block_start".asJson, "index" -> i.asJson, "content_block" -> b.asJson)
    case ContentBlockDelta(i, d) =>
      Json.obj("type" -> "content_block_delta".asJson, "index" -> i.asJson, "delta" -> d.asJson)
    case ContentBlockStop(i) => Json.obj("type" -> "content_block_stop".asJson, "index" -> i.asJson)
    case MessageDelta(reason, seq, details, usage) =>
      Json.obj(
        "type"  -> "message_delta".asJson,
        "delta" -> Json
          .obj(
            "stop_reason"   -> reason.asJson,
            "stop_sequence" -> seq.asJson,
            "stop_details"  -> details.asJson
          )
          .dropNullValues,
        "usage" -> Json.fromJsonObject(usage)
      )
    case MessageStop     => Json.obj("type" -> "message_stop".asJson)
    case Unknown(_, raw) => Json.fromJsonObject(raw)
  }
}
