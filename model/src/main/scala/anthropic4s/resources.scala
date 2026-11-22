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
import io.circe.syntax._
import io.circe.{Decoder, Encoder, JsonObject}

import java.time.{Instant, OffsetDateTime}
import scala.annotation.unused

private[anthropic4s] object Timestamps {
  // The API sends RFC 3339 timestamps, with either `Z` or a numeric offset.
  implicit val instantDecoder: Decoder[Instant] = Decoder[OffsetDateTime].map(_.toInstant)
}

/** One page of a list endpoint. Use the `listAll` stream on a resource to walk every page. */
final case class Page[A] private[anthropic4s] (
    data: List[A],
    hasMore: Boolean,
    firstId: Option[String],
    lastId: Option[String]
)

object Page {
  implicit def decoder[A: Decoder]: Decoder[Page[A]] = Decoder.instance { c =>
    for {
      data    <- c.get[Option[List[A]]]("data")
      hasMore <- c.get[Option[Boolean]]("has_more")
      first   <- c.get[Option[String]]("first_id")
      last    <- c.get[Option[String]]("last_id")
    } yield new Page(data.getOrElse(Nil), hasMore.getOrElse(false), first, last)
  }
}

/** A model the API serves, with its limits. */
final case class ModelInfo private (
    id: Model,
    displayName: Option[String],
    createdAt: Option[Instant],
    maxInputTokens: Option[Long],
    maxTokens: Option[Long],
    extras: JsonObject
)

object ModelInfo {
  import Timestamps.instantDecoder

  @unused private def unapply(m: ModelInfo): Some[ModelInfo] = Some(m)

  implicit val decoder: Decoder[ModelInfo] = Wire.decoder { r =>
    r.skip("type")
    for {
      id      <- r.required[Model]("id")
      name    <- r.optional[String]("display_name")
      created <- r.optional[Instant]("created_at")
      maxIn   <- r.optional[Long]("max_input_tokens")
      maxOut  <- r.optional[Long]("max_tokens")
    } yield new ModelInfo(id, name, created, maxIn, maxOut, r.extras)
  }
}

// -- Message Batches ---------------------------------------------------------------------------------

/** One request of a batch. `customId` is how you match the result later; results come back in any order. */
final case class BatchRequest(customId: String, params: MessageRequest)

object BatchRequest {
  implicit val encoder: Encoder[BatchRequest] =
    Encoder.instance(b => JsonObject("custom_id" -> b.customId.asJson, "params" -> b.params.asJson).asJson)
}

final case class BatchRequestCounts private (
    processing: Long,
    succeeded: Long,
    errored: Long,
    canceled: Long,
    expired: Long
)

object BatchRequestCounts {
  @unused private def unapply(c: BatchRequestCounts): Some[BatchRequestCounts] = Some(c)

  implicit val decoder: Decoder[BatchRequestCounts] = Decoder.instance { c =>
    def n(key: String) = c.get[Option[Long]](key).map(_.getOrElse(0L))
    for {
      p  <- n("processing")
      s  <- n("succeeded")
      e  <- n("errored")
      ca <- n("canceled")
      ex <- n("expired")
    } yield new BatchRequestCounts(p, s, e, ca, ex)
  }
}

/** A message batch and its progress. */
final case class MessageBatch private (
    id: String,
    processingStatus: BatchStatus,
    requestCounts: BatchRequestCounts,
    createdAt: Option[Instant],
    expiresAt: Option[Instant],
    endedAt: Option[Instant],
    resultsUrl: Option[String],
    extras: JsonObject
) {
  def hasEnded: Boolean = processingStatus == BatchStatus.Ended
}

object MessageBatch {
  import Timestamps.instantDecoder

  @unused private def unapply(b: MessageBatch): Some[MessageBatch] = Some(b)

  implicit val decoder: Decoder[MessageBatch] = Wire.decoder { r =>
    r.skip("type")
    for {
      id      <- r.required[String]("id")
      status  <- r.required[BatchStatus]("processing_status")
      counts  <- r.required[BatchRequestCounts]("request_counts")
      created <- r.optional[Instant]("created_at")
      expires <- r.optional[Instant]("expires_at")
      ended   <- r.optional[Instant]("ended_at")
      results <- r.optional[String]("results_url")
    } yield new MessageBatch(id, status, counts, created, expires, ended, results, r.extras)
  }
}

/** What happened to one request of a batch. */
sealed trait BatchOutcome extends Product with Serializable
object BatchOutcome {
  final case class Succeeded(message: Message)           extends BatchOutcome
  final case class Errored(error: ApiErrorBody)          extends BatchOutcome
  case object Canceled                                   extends BatchOutcome
  case object Expired                                    extends BatchOutcome
  final case class Unknown(tpe: String, raw: JsonObject) extends BatchOutcome

  implicit val decoder: Decoder[BatchOutcome] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "succeeded" => c.get[Message]("message").map(Succeeded(_))
      case "errored"   => c.get[ApiErrorBody]("error").map(Errored(_))
      case "canceled"  => Right(Canceled)
      case "expired"   => Right(Expired)
      case other       => c.as[JsonObject].map(Unknown(other, _))
    }
  }
}

/** One line of a batch's results. */
final case class BatchResult(customId: String, outcome: BatchOutcome)

object BatchResult {
  implicit val decoder: Decoder[BatchResult] = Decoder.instance { c =>
    for { id <- c.get[String]("custom_id"); o <- c.get[BatchOutcome]("result") } yield BatchResult(id, o)
  }
}

// -- Files ---------------------------------------------------------------------------------------------

/** A file stored with the Files API. Reference it in a request with `Source.File(id)`. */
final case class FileMetadata private (
    id: String,
    filename: String,
    mimeType: String,
    sizeBytes: Long,
    createdAt: Option[Instant],
    downloadable: Option[Boolean],
    extras: JsonObject
)

object FileMetadata {
  import Timestamps.instantDecoder

  @unused private def unapply(f: FileMetadata): Some[FileMetadata] = Some(f)

  implicit val decoder: Decoder[FileMetadata] = Wire.decoder { r =>
    r.skip("type")
    for {
      id       <- r.required[String]("id")
      filename <- r.required[String]("filename")
      mime     <- r.required[String]("mime_type")
      size     <- r.required[Long]("size_bytes")
      created  <- r.optional[Instant]("created_at")
      dl       <- r.optional[Boolean]("downloadable")
    } yield new FileMetadata(id, filename, mime, size, created, dl, r.extras)
  }
}
