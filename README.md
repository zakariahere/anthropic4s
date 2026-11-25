<p align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="docs/assets/logo-mark-dark.svg">
    <img src="docs/assets/logo-mark.svg" width="96" alt="anthropic4s">
  </picture>
</p>

<h1 align="center">anthropic4s</h1>

<p align="center">A pure-Scala SDK for the Claude API, built on Cats Effect and fs2.</p>

> **Unofficial.** This is a community project. It is not affiliated with or endorsed by Anthropic.
> **Status:** 1.1.0, on Maven Central for Scala 2.13; see [Installing](#installing).

anthropic4s talks to the Claude API directly. It does not wrap the official Java SDK, so it brings no
Kotlin runtime and no Jackson version to pin. What you get instead:

- **Case classes in, case classes out.** Structured outputs and tool arguments are derived from your types.
- **A typed agent loop.** Tools are functions; the runner calls them, in parallel, until the model is done.
- **Streaming as `fs2.Stream`**, resource-safe and cancellable.
- **Typed errors, retries and timeouts** that match the official SDKs.
- **OpenTelemetry tracing** through otel4s.
- **Play-friendly.** A `Future` facade, and structured outputs driven by your Play JSON `Format`s.

## Quick start

```scala
import anthropic4s._
import cats.effect.{IO, IOApp}

object Hello extends IOApp.Simple {
  def run: IO[Unit] =
    Anthropic4s.fromEnv[IO].use { client =>            // reads ANTHROPIC_API_KEY
      val request = MessageRequest(Model.ClaudeOpus5_5, maxTokens = 16000, MessageParam.user("Why do cats purr?"))
      client.messages.create(request).flatMap(reply => IO.println(reply.text))
    }
}
```

`Anthropic4s.fromEnv[IO]` is a `Resource`: the block inside `use` runs with an open client, and the
connections are closed afterwards, even if it fails. Create one client and share it.

Every snippet in this README comes from [`examples/`](examples/src/main/scala/anthropic4s/examples),
which is compiled with the build.

## Streaming

```scala
import anthropic4s.syntax._

client.messages
  .stream(request)
  .textDeltas                       // just the text, piece by piece
  .evalMap(IO.print)
  .compile
  .drain

client.messages.stream(request).finalMessage   // or: stream, and keep only the complete reply
```

Stopping early (`take`, cancellation, a timeout around it) closes the connection.

## Structured outputs

Ask for a case class and get one back. The JSON Schema is derived from the type and sent as the response
format, so the API guarantees a reply that fits.

```scala
import anthropic4s.schema.description

sealed trait Difficulty
object Difficulty { case object Easy extends Difficulty; case object Hard extends Difficulty }

final case class Recipe(
    name: String,
    @description("Total time in minutes") minutes: Int,
    difficulty: Difficulty,
    pairing: Option[String]
)

client.messages.parse[Recipe](request).map(_.value)   // IO[Recipe]
```

No instance to declare: case classes and sealed traits get a `Schema` automatically. To avoid deriving it
at every use, cache it: `object Recipe { implicit val schema: Schema[Recipe] = Schema.derived }`.

**With Play JSON**, add `anthropic4s-play-json` and pair the schema with your `Format`:

```scala
object Recipe {
  implicit val config: JsonConfiguration = PlaySchema.configuration
  implicit val format: OFormat[Recipe]   = Json.format[Recipe]
  implicit val schema: Schema[Recipe]    = PlaySchema.derived[Recipe]
}
```

## Tools and agents

A tool is a function from a case class to an effect.

```scala
final case class GetWeather(@description("City name") city: String)

val weather = Tool("get_weather", "Current weather for a city") { (in: GetWeather) =>
  weatherService.lookup(in.city)            // IO[String], IO[SomeCaseClass], ...
}

val runner = client.messages.toolRunner(List(weather))

runner.run(request)       // IO[RunResult]: the final reply, the transcript, total usage, why it stopped
runner.events(request)    // Stream[IO, RunEvent]: text deltas, tool starts and results, as they happen
```

- Tool calls of one turn run concurrently, and their results go back in order in one message.
- A tool that throws, or gets bad arguments, becomes an error result the model can react to.
- `RunnerConfig` adds an approval gate (`withBeforeTool`), an iteration limit and request hooks.
- How a run ended is a value (`Completed`, `MaxIterations`, `Refused`, `MaxTokens`, `Aborted`), not an exception.

## Errors, retries and timeouts

Failures are raised in `F` as subtypes of `AnthropicError`:

```scala
client.messages.create(request).recover {
  case e: AnthropicError.RateLimited => s"retry in ${e.retryAfter}"
  case e: AnthropicError.Api         => s"${e.status} ${e.errorType}: ${e.errorMessage} (${e.requestId})"
  case _: AnthropicError.Timeout     => "timed out"
}
```

Connection errors, timeouts, 408, 409, 429 and 5xx are retried twice with exponential backoff, honouring
the server's `retry-after` and `x-should-retry` headers. A request is never retried once a response has
started. Change the defaults on `AnthropicConfig` or per call with `RequestOptions`.

## Tracing

Add `anthropic4s-otel4s` and pass a telemetry to the client:

```scala
for {
  telemetry <- OtelTelemetry[IO](otel.tracerProvider)
  client     = Anthropic4s.resource[IO](config, telemetry)
} yield client
```

Each API request is a `CLIENT` span (retries included); an agent run is one tree with a span per request
and per tool call. Model, token usage, stop reason and request ids are recorded as `gen_ai.*` and
`anthropic.*` attributes. Prompts and replies are recorded only if you opt in with `ContentMode.Content`.

## From Play, with `Future`

```scala
@Singleton
class Claude @Inject() (lifecycle: ApplicationLifecycle)(implicit ec: ExecutionContext) {
  private val anthropic = FutureAnthropic.fromEnv()
  lifecycle.addStopHook(() => anthropic.close())

  def ask(question: String): Future[String] =
    anthropic.create(MessageRequest(Model.ClaudeOpus5_5, 16000, MessageParam.user(question))).map(_.text)
}
```

See [docs/play.md](docs/play.md) for streaming, tools and structured outputs from Play.

## Testing your agents

`anthropic4s-testkit` has a scripted model that needs no network:

```scala
for {
  model  <- ScriptedModel.of[IO](Replies.toolUse("get_weather", Json.obj("city" -> "Ghent".asJson)), Replies.text("Rainy."))
  result <- model.toolRunner(List(weather)).run(request)
  sent   <- model.requests            // inspect what your code sent
} yield assertEquals(result.text, "Rainy.")
```

## Decision models

`anthropic4s-decide` talks to a **decision model**: a small model that picks an answer and returns
calibrated probabilities, instead of writing text. It speaks the System One protocol (`POST
/v1/systemone`), served for example by llama-server running Kev-4B. Typical answers take tens of
milliseconds, and no output tokens. The module does not depend on the rest of anthropic4s.

```scala
import anthropic4s.decide._

val team = Question.choice[Team]("Which team should handle this?")(
  Criterion(Team.Billing, "billing", "payments, charges, refunds, invoices"),
  Criterion(Team.Shipping, "shipping", "delivery, tracking, lost or late parcels"),
  Criterion(Team.Technical, "technical", "bugs, errors, login problems")
)
val angry   = Question.noul("Is the customer angry?")                // a yes/no question
val urgency = Question.score("How urgent is this?")("can wait", "this week", "today", "right now")

Decide.ember[IO](DecideConfig.local()).use { decide =>               // llama-server on localhost:8081
  decide.ask(ticket, team)                                           // IO[ChoiceAnswer[Team]]
  decide.run(ticket, (team.as("team"), angry.as("angry"), urgency.as("urgency")).tupled)
  // one request: IO[(ChoiceAnswer[Team], Double, ScoreAnswer)]
}
```

- Each question has its own answer type: a choice gives the chosen value, the probability of every option
  and a `confidence`; a score gives `nearest` and `mostLikely` level; a yes/no question gives a probability.
- Any server, not only localhost: `DecideConfig(uri"https://decide.example.com")`, with `withApiKey` and
  `withTimeout`. Or `DecideConfig.fromEnv[IO]`, which reads the optional `DECIDE_BASE_URL`,
  `DECIDE_API_KEY` and `DECIDE_TIMEOUT` (e.g. `30s`), and falls back to `localhost:8081` when none is set.
- Already using anthropic4s? Pass the same http4s client to both: `Decide.fromClient(http, config)`.
- Failures are `DecideError`s. Invalid questions (two options with the same label, a score outside 2 to 10
  levels) fail before anything is sent.
- From Play, `FutureDecide.fromEnv()` returns `Future`s; see [docs/play.md](docs/play.md#decision-models).

**With an agent**, let the decision model make the quick calls and Claude do the thinking. Route each
request to the smallest setup that can handle it, and check risky tool calls before they run:

```scala
RunnerConfig.default[IO].withBeforeTool { call =>
  if (call.name != "book_table") IO.pure(ToolDecision.Allow)
  else
    decide.ask(conversation(userText), bookingAsked).map { p =>
      if (p >= 0.5) ToolDecision.Allow
      else ToolDecision.Deny("The user did not ask for a booking. Do not book anything.")
    }
    .handleError(_ => ToolDecision.Deny("Bookings cannot be checked right now."))   // fail closed
}
```

A denied call never runs; Claude gets the reason as a tool error and carries on. The full router and guard
are in [`AgentDecisions`](examples/src/main/scala/anthropic4s/examples/AgentDecisions.scala). Two things
learned while tuning them:

- **Describe what an option needs, not what it is about.** "Should I eat outside tonight?" never says
  "weather"; a criterion like "needs today's weather to answer" catches it.
- **Confidence is not the top probability.** It is `(p − 1/n) / (1 − 1/n)`, so with three options a 0.53
  favourite is only 0.29 confident. Pick thresholds per question.

## Modules

| Artifact | What it is |
|---|---|
| `anthropic4s` | The one most applications need: the client on http4s Ember, plus `FutureAnthropic` |
| `anthropic4s-core` | The client on any http4s `Client`, the tool runner, the telemetry SPI |
| `anthropic4s-model` | Request and response types with their JSON codecs. No Cats Effect |
| `anthropic4s-schema` | `Schema[A]`: JSON Schema, decoder and encoder from one derivation |
| `anthropic4s-play-json` | Structured outputs and tool arguments from Play JSON `Format`s |
| `anthropic4s-otel4s` | OpenTelemetry tracing |
| `anthropic4s-testkit` | `ScriptedModel` and reply builders for tests |
| `anthropic4s-decide` | A client for System One decision models, plus `FutureDecide`. Usable on its own (new in 1.1) |

Scala 2.13, JDK 17+. Covered: Messages (create, stream, count tokens), tools, structured outputs,
Batches, Files, Models. Not yet: Bedrock / Vertex / Foundry, Managed Agents, the Admin API, Scala 3.

## Design notes

- **Forward compatible.** The API ships new fields and block types constantly. Unknown content blocks and
  stream events are kept and sent back untouched; every object carries unmodeled fields in `extras`; enums
  are open (`Model.of("...")`, `StopReason.of("...")`). `request.withExtra(key, json)` sends anything this
  SDK does not model yet.
- **Binary compatible by construction.** SDK types have private constructors and `withX` methods, so a new
  API field does not break code compiled against an older version. Your own case classes are untouched.
- **One type tree.** Beta features are flags (`RequestOptions.withBetas`), not a parallel set of types.
- **No sampling parameters.** `temperature`, `top_p` and `top_k` are rejected by the current API version,
  so they are not modeled.

## Installing

sbt:

```scala
libraryDependencies += "io.github.zakariahere" %% "anthropic4s" % "1.1.0"
```

Maven:

```xml
<dependency>
  <groupId>io.github.zakariahere</groupId>
  <artifactId>anthropic4s_2.13</artifactId>
  <version>1.1.0</version>
</dependency>
```

Optional modules, same group and version: `anthropic4s-play-json`, `anthropic4s-otel4s`,
`anthropic4s-testkit`, `anthropic4s-decide` (since 1.1.0). Versions follow early-semver: every 1.x release
is binary compatible with 1.0.0, and MiMa checks that in CI.

## Development

```
sbt test                 # everything, no network and no API key needed
sbt "examples/runMain anthropic4s.examples.Hello"     # needs ANTHROPIC_API_KEY, uses real tokens
sbt "examples/runMain anthropic4s.examples.FutureDecisions"   # needs llama-server with Kev-4B on :8081, or DECIDE_BASE_URL
sbt prePR                # format, license headers, CI workflow
```

## License

Apache 2.0. See [LICENSE](LICENSE).
