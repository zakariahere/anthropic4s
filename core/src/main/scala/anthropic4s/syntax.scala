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

import cats.effect.Concurrent
import cats.syntax.all._
import fs2.Stream

/**
 * Extension methods. `import anthropic4s.syntax._` adds helpers to a stream of events:
 * {{{
 * client.messages.stream(request).textDeltas.evalMap(IO.print).compile.drain   // print as it arrives
 * client.messages.stream(request).finalMessage                                 // just the complete reply
 * client.messages.stream(request).evalTap(render).finalMessage                 // both
 * }}}
 */
object syntax {

  implicit final class StreamEventOps[F[_]](private val events: Stream[F, StreamEvent]) extends AnyVal {

    /** Only the text, piece by piece, as the model writes it. */
    def textDeltas: Stream[F, String] =
      events.collect { case StreamEvent.ContentBlockDelta(_, Delta.Text(text)) => text }

    /** Runs the stream to the end and returns the complete reply. */
    def finalMessage(implicit F: Concurrent[F]): F[Message] =
      events
        .evalScan(MessageAccumulator.empty)((acc, event) => F.fromEither(acc.add(event)))
        .compile
        .lastOrError
        .flatMap(acc => F.fromEither(acc.result))

    /**
     * The reply so far, after every event. Each element costs O(size), so prefer `textDeltas` for display.
     */
    def snapshots(implicit F: Concurrent[F]): Stream[F, Message] =
      events
        .evalScan(MessageAccumulator.empty)((acc, event) => F.fromEither(acc.add(event)))
        .mapFilter(_.partial)
  }
}
