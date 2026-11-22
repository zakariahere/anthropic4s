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

import anthropic4s.internal.{Call, Executor, Paging}
import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.all._
import cats.~>
import fs2.Stream
import io.circe.Json
import io.circe.syntax._
import org.http4s.headers.`Content-Type`
import org.http4s.multipart.{Multiparts, Part}
import org.http4s.{MediaType, Method}

import scala.concurrent.duration._

/**
 * Message Batches: many requests processed asynchronously, at half the price, usually within an hour.
 * {{{
 * for {
 *   batch <- client.batches.create(NonEmptyList.of(BatchRequest("q1", request1), BatchRequest("q2", request2)))
 *   _     <- client.batches.awaitEnded(batch.id)
 *   _     <- client.batches.results(batch.id).evalMap(handle).compile.drain
 * } yield ()
 * }}}
 */
trait Batches[F[_]] {
  def create(requests: NonEmptyList[BatchRequest]): F[MessageBatch]
  def retrieve(id: String): F[MessageBatch]
  def list(params: ListParams): F[Page[MessageBatch]]
  def list: F[Page[MessageBatch]] = list(ListParams.default)

  /** Every batch, fetching further pages as the stream is consumed. */
  def listAll: Stream[F, MessageBatch]

  /** Asks for a batch to stop. Requests already running may still finish. */
  def cancel(id: String): F[MessageBatch]

  /** Deletes an ended batch and its results. */
  def delete(id: String): F[Unit]

  /**
   * The results of an ended batch, one per request, streamed as they are read. They arrive in no particular
   * order: match them to your requests by `customId`.
   */
  def results(id: String): Stream[F, BatchResult]

  /** Polls until the batch has ended and returns it. Cancel the effect to stop waiting. */
  def awaitEnded(id: String, pollInterval: FiniteDuration): F[MessageBatch]
  def awaitEnded(id: String): F[MessageBatch] = awaitEnded(id, 30.seconds)

  def mapK[G[_]](fk: F ~> G): Batches[G] = {
    val self = this
    new Batches[G] {
      def create(requests: NonEmptyList[BatchRequest]): G[MessageBatch] = fk(self.create(requests))
      def retrieve(id: String): G[MessageBatch]                         = fk(self.retrieve(id))
      def list(params: ListParams): G[Page[MessageBatch]]               = fk(self.list(params))
      def listAll: Stream[G, MessageBatch]                              = self.listAll.translate(fk)
      def cancel(id: String): G[MessageBatch]                           = fk(self.cancel(id))
      def delete(id: String): G[Unit]                                   = fk(self.delete(id))
      def results(id: String): Stream[G, BatchResult]                   = self.results(id).translate(fk)
      def awaitEnded(id: String, pollInterval: FiniteDuration): G[MessageBatch] = fk(
        self.awaitEnded(id, pollInterval)
      )
    }
  }
}

/**
 * The Files API: upload a file once and refer to it by id in many requests.
 * {{{
 * for {
 *   file  <- client.files.upload("report.pdf", "application/pdf", Files[IO].readAll(path))
 *   reply <- client.messages.create(request.addMessages(MessageParam.user(
 *              ContentBlock.Document(Source.File(file.id)), ContentBlock.Text("Summarise this."))))
 * } yield reply
 * }}}
 */
trait Files[F[_]] {

  /** Uploads `content` as a file. The bytes are streamed, never held in memory as a whole. */
  def upload(filename: String, mimeType: String, content: Stream[F, Byte]): F[FileMetadata]
  def retrieveMetadata(id: String): F[FileMetadata]
  def list(params: ListParams): F[Page[FileMetadata]]
  def list: F[Page[FileMetadata]] = list(ListParams.default)
  def listAll: Stream[F, FileMetadata]

  /** The file's bytes. Only files the API marks as downloadable (those created by tools) can be fetched. */
  def download(id: String): Stream[F, Byte]
  def delete(id: String): F[Unit]
}

final private[anthropic4s] class BatchesImpl[F[_]](executor: Executor[F], telemetry: Telemetry[F])(implicit
    F: Async[F]
) extends Batches[F] {

  private val base = "/v1/messages/batches"

  private def call[A: io.circe.Decoder](
      operation: String,
      method: Method,
      route: String,
      body: Option[Json] = None,
      query: List[(String, String)] = Nil
  ): F[A] =
    executor
      .json[A](
        Call[F](
          s"messages.batches.$operation",
          method,
          route,
          RequestOptions.default,
          query = query,
          body = body
        ),
        telemetry
      )((_, _) => F.unit)
      .map(_.value)

  def create(requests: NonEmptyList[BatchRequest]): F[MessageBatch] =
    call[MessageBatch]("create", Method.POST, base, Some(Json.obj("requests" -> requests.asJson)))

  def retrieve(id: String): F[MessageBatch] = call[MessageBatch]("retrieve", Method.GET, s"$base/$id")
  def list(params: ListParams): F[Page[MessageBatch]] =
    call[Page[MessageBatch]]("list", Method.GET, base, query = params.query)
  def listAll: Stream[F, MessageBatch]    = Paging.all(ListParams.default)(list)
  def cancel(id: String): F[MessageBatch] = call[MessageBatch]("cancel", Method.POST, s"$base/$id/cancel")
  def delete(id: String): F[Unit]         = call[Json]("delete", Method.DELETE, s"$base/$id").void

  def results(id: String): Stream[F, BatchResult] =
    executor
      .open(
        Call[F]("messages.batches.results", Method.GET, s"$base/$id/results", RequestOptions.default),
        telemetry
      )
      .flatMap { case (_, response) =>
        // One JSON document per line (JSONL).
        response.body.through(fs2.text.utf8.decode).through(fs2.text.lines).filter(_.trim.nonEmpty).evalMap {
          line =>
            io.circe.parser.decode[BatchResult](line) match {
              case Right(result) => F.pure(result)
              case Left(e) => F.raiseError[BatchResult](new AnthropicError.Decoding(e.getMessage, line, e))
            }
        }
      }

  def awaitEnded(id: String, pollInterval: FiniteDuration): F[MessageBatch] =
    retrieve(id).flatMap(batch =>
      if (batch.hasEnded) F.pure(batch) else F.sleep(pollInterval) *> awaitEnded(id, pollInterval)
    )
}

final private[anthropic4s] class FilesImpl[F[_]](executor: Executor[F], telemetry: Telemetry[F])(implicit
    F: Async[F]
) extends Files[F] {

  private val base = "/v1/files"

  def upload(filename: String, mimeType: String, content: Stream[F, Byte]): F[FileMetadata] =
    for {
      mediaType <- F.fromEither(
        MediaType
          .parse(mimeType)
          .leftMap(_ => new AnthropicError.Configuration(s"invalid MIME type: $mimeType"))
      )
      multiparts <- Multiparts.forSync[F]
      multipart  <- multiparts.multipart(
        Vector(Part.fileData[F]("file", filename, content, `Content-Type`(mediaType)))
      )
      response <- executor.json[FileMetadata](
        Call[F](
          "files.upload",
          Method.POST,
          base,
          RequestOptions.default,
          // The multipart boundary lives in its Content-Type header, so those headers must be kept.
          customize =
            req => multipart.headers.headers.foldLeft(req.withEntity(multipart))((r, h) => r.putHeaders(h))
        ),
        telemetry
      )((_, _) => F.unit)
    } yield response.value

  private def get[A: io.circe.Decoder](
      operation: String,
      route: String,
      query: List[(String, String)] = Nil
  ): F[A] =
    executor
      .json[A](
        Call[F](s"files.$operation", Method.GET, route, RequestOptions.default, query = query),
        telemetry
      )((_, _) => F.unit)
      .map(_.value)

  def retrieveMetadata(id: String): F[FileMetadata]   = get[FileMetadata]("retrieve_metadata", s"$base/$id")
  def list(params: ListParams): F[Page[FileMetadata]] = get[Page[FileMetadata]]("list", base, params.query)
  def listAll: Stream[F, FileMetadata]                = Paging.all(ListParams.default)(list)

  def download(id: String): Stream[F, Byte] =
    executor
      .open(Call[F]("files.download", Method.GET, s"$base/$id/content", RequestOptions.default), telemetry)
      .flatMap { case (_, response) => response.body }

  def delete(id: String): F[Unit] =
    executor
      .json[Json](Call[F]("files.delete", Method.DELETE, s"$base/$id", RequestOptions.default), telemetry)(
        (_, _) => F.unit
      )
      .void
}
