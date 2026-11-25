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

import cats.effect.{Async, Resource, Temporal}
import cats.syntax.all._
import fs2.io.net.Network
import io.circe.{Json, JsonObject}
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.headers.Authorization
import org.http4s.{AuthScheme, Credentials, Method, Request}

import scala.concurrent.duration.Duration

/** A client for a System One decision endpoint: llama-server running Kev-4B, TypeSafe's Jev, or any other. */
trait Decide[F[_]] {

  /** Sends every question of the batch in one request. */
  def run[A](state: Json, batch: Batch[A]): F[A]
  def run[A](state: String, batch: Batch[A]): F[A] = run(Json.fromString(state), batch)

  /** One question, one request. Several `ask`s with `parTupled` can beat one batch, so measure both. */
  def ask[A](state: Json, question: Question[A]): F[A]   = run(state, question.as("q"))
  def ask[A](state: String, question: Question[A]): F[A] = ask(Json.fromString(state), question)
}

object Decide {

  /**
   * A client with its own Ember connection pool, for when you use the decision model alone. If you also use
   * anthropic4s, prefer [[fromClient]] with the HTTP client you already have: one pool for both.
   */
  def ember[F[_]: Async: Network](config: DecideConfig): Resource[F, Decide[F]] =
    EmberClientBuilder
      .default[F]
      // Ember's own 45 s request limit is switched off: `config.timeout` is the one that applies.
      .withTimeout(Duration.Inf)
      .build
      .map(fromClient(_, config))

  /** Builds a client on any http4s `Client`, the same way `AnthropicClient.fromClient` does. */
  def fromClient[F[_]](http: Client[F], config: DecideConfig)(implicit F: Temporal[F]): Decide[F] =
    new Decide[F] {

      def run[A](state: Json, batch: Batch[A]): F[A] =
        validate(batch) *>
          F.timeoutTo(
            // A refused connection, a DNS failure, a reset: the server could not be reached.
            send(state, batch).adaptError {
              case e: DecideError => e
              case e              => new DecideError.Unavailable(config.endpoint.renderString, e)
            },
            config.timeout,
            F.raiseError(new DecideError.Timeout(config.timeout))
          )

      private def validate(batch: Batch[_]): F[Unit] = {
        val ids      = batch.questions.map(_._1)
        val problems =
          batch.questions.flatMap(_._2.problem) ++
            Option.when(ids.isEmpty)("the batch has no questions") ++
            Option.when(ids.distinct.size != ids.size)(
              s"duplicate question ids: ${ids.diff(ids.distinct).distinct.mkString(", ")}"
            )
        F.raiseWhen(problems.nonEmpty)(new DecideError.InvalidQuestion(problems.mkString("; ")))
      }

      private def send[A](state: Json, batch: Batch[A]): F[A] = {
        val body = Json.obj(
          "state"     -> state,
          "questions" -> Json.fromFields(batch.questions.map { case (id, q) => id -> q.toJson })
        )
        val plain   = Request[F](Method.POST, config.endpoint).withEntity(body)
        val request = config.apiKey.fold(plain) { key =>
          plain.putHeaders(Authorization(Credentials.Token(AuthScheme.Bearer, key)))
        }

        http.run(request).use { response =>
          if (response.status.isSuccess)
            response.as[Json].flatMap(json => F.fromEither(readAnswers(json, batch)))
          else
            response.bodyText.compile.string.flatMap { text =>
              F.raiseError(new DecideError.Http(response.status.code, text.take(500)))
            }
        }
      }
    }

  // ASSUMPTION: answers come back under a top-level "answers" object keyed by question id.
  // Check this against a real llama-server response before relying on it.
  private def readAnswers[A](json: Json, batch: Batch[A]): Either[DecideError, A] =
    json.hcursor
      .get[JsonObject]("answers")
      .flatMap(batch.read)
      .leftMap(failure => new DecideError.Decode(failure.getMessage))
}
