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
import cats.effect.{Async, Deferred, Ref}
import cats.syntax.all._
import fs2.{Chunk, Stream}
import org.http4s.ServerSentEvent

import scala.concurrent.duration._

private[anthropic4s] object Sse {

  /**
   * Turns the raw bytes of a streaming response into [[StreamEvent]]s.
   *
   *   - `ping` events only prove the connection is alive; they are dropped.
   *   - An `error` event fails the stream with [[AnthropicError.StreamFailure]].
   *   - `message_stop` is the last event emitted. A body that ends before it fails with
   *     [[AnthropicError.StreamTruncated]].
   *   - `idle` is the longest silence tolerated between two events.
   */
  def events[F[_]](body: Stream[F, Byte], idle: FiniteDuration, scope: RequestScope[F])(implicit
      F: Async[F]
  ): Stream[F, StreamEvent] =
    Stream.eval((Ref.of[F, Boolean](false), Deferred[F, Unit]).tupled).flatMap { case (finished, stopSeen) =>
      def decode(sse: ServerSentEvent): F[Option[StreamEvent]] =
        sse.eventType match {
          case Some("ping")  => F.pure(None)
          case Some("error") =>
            val error = sse.data.flatMap(d => io.circe.parser.decode[ApiErrorBody](d).toOption)
            F.raiseError(
              new AnthropicError.StreamFailure(
                error.fold("unknown_error")(_.errorType),
                error.map(_.message).orElse(sse.data).getOrElse("")
              )
            )
          case _ =>
            sse.data.filter(_.trim.nonEmpty) match {
              case None       => F.pure(None)
              case Some(data) =>
                io.circe.parser.decode[StreamEvent](data) match {
                  case Left(e)      => F.raiseError(new AnthropicError.Decoding(e.getMessage, data, e))
                  case Right(event) =>
                    val mark =
                      if (event == StreamEvent.MessageStop) finished.set(true) *> stopSeen.complete(()).void
                      else F.unit
                    (scope.streamEvent(event) *> mark).as(Option(event))
                }
            }
        }

      val decoded: Stream[F, StreamEvent] =
        body
          .through(ServerSentEvent.decoder[F])
          .timeoutOnPullTo(idle, Stream.raiseError[F](new AnthropicError.Timeout(idle)))
          // Anything after message_stop is ignored; we only keep reading to reach the end of the body.
          .evalMapFilter(sse =>
            finished.get.flatMap(done => if (done) F.pure(Option.empty[StreamEvent]) else decode(sse))
          )
          // Reading to the end of the body lets the HTTP client reuse the connection. The API closes the
          // body right after message_stop; the grace period only matters for a peer that does not.
          .interruptWhen((stopSeen.get *> F.sleep(EndOfBodyGrace)).attempt)

      decoded ++ Stream.exec(
        finished.get.flatMap(done => F.raiseUnless(done)(new AnthropicError.StreamTruncated))
      )
    }

  private val EndOfBodyGrace: FiniteDuration = 1.second

  /** Server-sent event framing for a list of JSON documents. Used by tests and fakes. */
  def render(events: List[(String, String)]): Stream[fs2.Pure, Byte] =
    Stream.chunk(
      Chunk.array(
        events.map { case (name, data) => s"event: $name\ndata: $data\n\n" }.mkString.getBytes("UTF-8")
      )
    )
}

private[anthropic4s] object Paging {

  /** Walks every page of a list endpoint by following `last_id` while `has_more` is set. */
  def all[F[_]: cats.Functor, A](first: ListParams)(fetch: ListParams => F[Page[A]]): Stream[F, A] =
    Stream
      .unfoldLoopEval(first) { params =>
        fetch(params).map { page =>
          val next = if (page.hasMore) page.lastId.map(params.withAfterId) else None
          (page.data, next)
        }
      }
      .flatMap(Stream.emits(_))
}
