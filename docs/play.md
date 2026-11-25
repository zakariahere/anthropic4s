# Using anthropic4s from Play

Play code works with `Future`; the SDK is written against Cats Effect. `FutureAnthropic` sits between the
two, so a Play application never has to touch `IO`.

```scala
libraryDependencies ++= Seq(
  "io.github.zakariahere" %% "anthropic4s"           % anthropic4sVersion,
  "io.github.zakariahere" %% "anthropic4s-play-json" % anthropic4sVersion
)
```

Unlike the Java SDK, this needs no `dependencyOverrides`: there is no Kotlin runtime and no Jackson module
to keep in step with Play's.

## One client for the application

```scala
import anthropic4s._
import javax.inject.{Inject, Singleton}
import play.api.inject.ApplicationLifecycle
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class Claude @Inject() (lifecycle: ApplicationLifecycle)(implicit ec: ExecutionContext) {

  // Reads ANTHROPIC_API_KEY. Use FutureAnthropic.start(AnthropicConfig(key)) to take it from Play config.
  private val anthropic = FutureAnthropic.fromEnv()
  lifecycle.addStopHook(() => anthropic.close())

  def ask(question: String): Future[String] =
    anthropic
      .create(MessageRequest(Model.ClaudeOpus5_5, 16000, MessageParam.user(question)))
      .map(_.text)
}
```

Starting the client opens no connection, so it is safe in a constructor.

## Errors

A failed call is a failed `Future` holding an `AnthropicError`. In a controller:

```scala
claude.ask(question).map(Ok(_)).recover {
  case _: AnthropicError.RateLimited => TooManyRequests("Busy, try again shortly")
  case e: AnthropicError.Api         => BadGateway(s"Claude said ${e.status}")
  case _: AnthropicError.Timeout     => GatewayTimeout
}
```

## Structured outputs with your `Format`

```scala
import anthropic4s.playjson.PlaySchema
import anthropic4s.schema.{description, Schema}
import play.api.libs.json._

final case class Ticket(@description("One-line summary") title: String, urgent: Boolean, assignee: Option[String])

object Ticket {
  implicit val config: JsonConfiguration = PlaySchema.configuration
  implicit val format: OFormat[Ticket]   = Json.format[Ticket]
  implicit val schema: Schema[Ticket]    = PlaySchema.derived[Ticket]
}

val ticket: Future[Ticket] = anthropic.parse[Ticket](request).map(_.value)
```

Three things to know:

- `PlaySchema.configuration` makes `Json.format` write a missing `Option` as `null` and tag sealed traits
  with a `type` field. That is the shape the schema promises the model will send.
- A sealed trait of case objects is described to the model as a list of strings. Give it a `Format` that
  reads strings: `PlaySchema.enumFormat("Low" -> Low, "High" -> High)`.
- `Json.format` cannot see the cases of a sealed trait when they are declared inside the same companion
  object that calls it. Declare the cases at the top level.

## Streaming to the browser

`stream` calls you back for every event and completes with the whole reply:

```scala
def chat(question: String) = Action {
  val (queue, source) = Source.queue[String](64).preMaterialize()
  anthropic
    .streamText(request(question))(text => queue.offer(text))
    .onComplete(_ => queue.complete())
  Ok.chunked(source)
}
```

The callback runs on the SDK's threads: hand the text off, do not block in it.

## Tools

Write tool handlers with `Future`:

```scala
final case class FindOrder(@description("Order number") id: String)

val findOrder = FutureAnthropic.tool("find_order", "Looks an order up by its number") { (in: FindOrder) =>
  orderRepository.find(in.id).map(_.fold("No such order")(_.summary))   // Future[String]
}

val answer: Future[String] = anthropic.runTools(request, List(findOrder)).map(_.text)
```

## Going further

`anthropic.client` is the full `AnthropicClient[IO]`, and `anthropic.run(io)` turns any SDK effect into a
`Future`. Batches, Files and Models are reached that way:

```scala
anthropic.run(anthropic.client.models.listAll.compile.toList)   // Future[List[ModelInfo]]
```

## Decision models

`anthropic4s-decide` has its own `Future` facade, `FutureDecide`, for classifying from a controller without
calling Claude. It does not need the rest of anthropic4s:

```scala
libraryDependencies += "io.github.zakariahere" %% "anthropic4s-decide" % anthropic4sVersion   // 1.1.0+
```

```scala
import anthropic4s.decide._
import cats.syntax.all._   // for .tupled on batches

@Singleton
class Triage @Inject() (lifecycle: ApplicationLifecycle)(implicit ec: ExecutionContext) {

  // Reads DECIDE_BASE_URL, DECIDE_API_KEY and DECIDE_TIMEOUT, all optional (default: localhost:8081).
  // To take them from Play config instead: FutureDecide.start(DecideConfig(uri).withApiKey(key)).
  private val decide = FutureDecide.fromEnv()
  lifecycle.addStopHook(() => decide.close())

  def classify(ticket: String): Future[JsObject] =
    decide
      .run(ticket, (team.as("team"), urgency.as("urgency")).tupled)   // one request, two answers
      .map { case (t, u) =>
        Json.obj("team" -> t.choice.toString, "urgency" -> u.mostLikely, "needsHuman" -> (t.confidence < 0.8))
      }
      // The decision model is a shortcut, not a dependency: if it is down, a human triages.
      .recover { case e: DecideError => Json.obj("error" -> e.getMessage, "needsHuman" -> true) }
}
```

The questions are defined as in the [README](../README.md#decision-models). A complete version is
[`FutureDecisions`](../examples/src/main/scala/anthropic4s/examples/FutureDecisions.scala).
