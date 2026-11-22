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

package anthropic4s.testkit

import anthropic4s._
import anthropic4s.schema.description
import cats.effect.testkit.TestControl
import cats.effect.{IO, Ref}
import io.circe.Json
import io.circe.syntax._
import munit.CatsEffectSuite

import scala.concurrent.duration._

final case class GetWeather(@description("City name") city: String)
final case class Forecast(city: String, celsius: Int)
final case class Wait(millis: Int, label: String)

class ToolRunnerSuite extends CatsEffectSuite {

  private val request = MessageRequest(Model.ClaudeOpus5_5, 1024, MessageParam.user("Weather in Brussels?"))

  private val weather: Tool[IO] =
    Tool("get_weather", "Current weather for a city")((in: GetWeather) => IO.pure(Forecast(in.city, 21)))

  private def call(city: String, id: String = "toolu_1"): Message =
    Replies.toolUse("get_weather", Json.obj("city" -> city.asJson), id)

  /** The tool results of the user message at the given position of a request. */
  private def results(request: MessageRequest, index: Int): List[ContentBlock.ToolResult] =
    request.messages.toList(index).content.blocks.collect { case r: ContentBlock.ToolResult => r }

  private def text(result: ContentBlock.ToolResult): String = result.content match {
    case Some(ToolResultContent.Text(t)) => t
    case other                           => other.toString
  }

  test("the loop runs the tool, sends the result back and returns the final answer") {
    for {
      model  <- ScriptedModel.of[IO](call("Brussels"), Replies.text("21 degrees in Brussels."))
      result <- model.toolRunner(List(weather)).run(request)
      sent   <- model.requests
    } yield {
      assertEquals(result.text, "21 degrees in Brussels.")
      assertEquals(result.stop, RunStop.Completed)
      assertEquals(result.iterations, 2)
      assertEquals(result.usage.outputTokens, 10L) // two replies of 5

      // The tool definition is derived, strict, and streams its arguments eagerly.
      val tool = sent.head.tools.collectFirst { case c: ToolDefinition.Custom => c }.get
      assertEquals(tool.name, "get_weather")
      assertEquals(tool.strict, Some(true))
      assertEquals(tool.eagerInputStreaming, Some(true))
      assertEquals(tool.inputSchema("required").map(_.noSpaces), Some("""["city"]"""))

      // Second request: user, assistant (the tool call), user (the result).
      assertEquals(sent(1).messages.toList.map(_.role), List(Role.User, Role.Assistant, Role.User))
      val sentBack = results(sent(1), 2)
      assertEquals(sentBack.map(_.toolUseId), List("toolu_1"))
      assertEquals(sentBack.map(text), List("""{"city":"Brussels","celsius":21}"""))
      assertEquals(result.transcript.length, 4)
    }
  }

  test("calls of one turn run concurrently, and all results go back in order in one message") {
    val slow: Tool[IO] = Tool("wait", "Waits")((in: Wait) => IO.sleep(in.millis.millis).as(in.label))
    val turn           = Replies.toolUses(
      ("a", "wait", Json.obj("millis" -> 300.asJson, "label" -> "first".asJson)),
      ("b", "wait", Json.obj("millis" -> 100.asJson, "label" -> "second".asJson)),
      ("c", "wait", Json.obj("millis" -> 200.asJson, "label" -> "third".asJson))
    )
    TestControl.executeEmbed {
      for {
        model <- ScriptedModel.of[IO](turn, Replies.text("ok"))
        start <- IO.monotonic
        _     <- model.toolRunner(List(slow)).run(request)
        end   <- IO.monotonic
        sent  <- model.requests
      } yield {
        assertEquals(end - start, 300.millis) // the longest call, not the sum
        assertEquals(sent(1).messages.length, 3)
        assertEquals(
          results(sent(1), 2).map(r => r.toolUseId -> text(r)),
          List("a" -> "first", "b" -> "second", "c" -> "third")
        )
      }
    }
  }

  test("a failing tool, bad arguments and an unknown tool all become error results, not crashes") {
    val failing: Tool[IO] =
      Tool("explode", "Fails")((_: GetWeather) => IO.raiseError[String](new RuntimeException("disk full")))
    val turn = Replies.toolUses(
      ("a", "explode", Json.obj("city" -> "x".asJson)),
      ("b", "get_weather", Json.obj("town" -> "x".asJson)),
      ("c", "teleport", Json.obj())
    )
    for {
      model <- ScriptedModel.of[IO](turn, Replies.text("ok"))
      _     <- model.toolRunner(List(failing, weather)).run(request)
      sent  <- model.requests
    } yield {
      val rs = results(sent(1), 2)
      assertEquals(rs.map(_.isError), List(true, true, true))
      assert(text(rs(0)).contains("disk full"))
      assert(text(rs(1)).contains("Invalid arguments"))
      assertEquals(text(rs(2)), "Unknown tool: teleport")
    }
  }

  test("beforeTool can deny, replace or abort") {
    def config(decision: ToolDecision) = RunnerConfig.default[IO].withBeforeTool(_ => IO.pure(decision))
    for {
      ran <- Ref.of[IO, Int](0)
      counted = Tool[IO, GetWeather, String]("get_weather", "Weather")(_ => ran.update(_ + 1).as("sunny"))
      m1     <- ScriptedModel.of[IO](call("x"), Replies.text("fine"))
      _      <- m1.toolRunner(List(counted), config(ToolDecision.Deny("not allowed here"))).run(request)
      denied <- m1.requests.map(rs => results(rs(1), 2).head)
      m2     <- ScriptedModel.of[IO](call("x"), Replies.text("fine"))
      _      <- m2
        .toolRunner(List(counted), config(ToolDecision.Replace(ToolResultContent.Text("cached: rain"))))
        .run(request)
      cached  <- m2.requests.map(rs => results(rs(1), 2).head)
      m3      <- ScriptedModel.of[IO](call("x"), Replies.text("never reached"))
      aborted <- m3.toolRunner(List(counted), config(ToolDecision.Abort("operator said stop"))).run(request)
      asked   <- m3.requests.map(_.size)
      runs    <- ran.get
    } yield {
      assert(denied.isError && text(denied) == "not allowed here")
      assert(!cached.isError && text(cached) == "cached: rain")
      assertEquals(aborted.stop, RunStop.Aborted("operator said stop"))
      assertEquals(asked, 1) // nothing was sent back after the abort
      assertEquals(runs, 0)  // the tool itself never ran in any of the three
    }
  }

  test("approval comes first: a denied call is never reported as started, and an abort runs nothing") {
    val turn = Replies.toolUses(
      ("a", "get_weather", Json.obj("city" -> "Ghent".asJson)),
      ("b", "get_weather", Json.obj("city" -> "Liege".asJson))
    )
    def gate(onB: ToolDecision) =
      RunnerConfig.default[IO].withBeforeTool(c => IO.pure(if (c.id == "b") onB else ToolDecision.Allow))
    for {
      ran <- Ref.of[IO, List[String]](Nil)
      counted = Tool[IO, GetWeather, String]("get_weather", "Weather")(in =>
        ran.update(_ :+ in.city).as("sunny")
      )
      m1         <- ScriptedModel.of[IO](turn, Replies.text("ok"))
      events     <- m1.toolRunner(List(counted), gate(ToolDecision.Deny("no"))).events(request).compile.toList
      afterDeny  <- ran.getAndSet(Nil)
      m2         <- ScriptedModel.of[IO](turn, Replies.text("unreachable"))
      aborted    <- m2.toolRunner(List(counted), gate(ToolDecision.Abort("stop"))).run(request)
      afterAbort <- ran.get
    } yield {
      assertEquals(events.collect { case RunEvent.ToolStarted(c) => c.id }, List("a"))
      assertEquals(
        events.collect { case RunEvent.ToolFinished(c, r) => c.id -> r.isError },
        List("a" -> false, "b" -> true)
      )
      assertEquals(afterDeny, List("Ghent"))
      assertEquals(aborted.stop, RunStop.Aborted("stop"))
      assertEquals(afterAbort, Nil) // call "a" was allowed, but nothing runs once any call aborts the turn
    }
  }

  test("the iteration limit ends a run that keeps calling tools") {
    for {
      model  <- ScriptedModel.of[IO](call("a"), call("b"), call("c"), Replies.text("unreachable"))
      result <- model.toolRunner(List(weather), RunnerConfig.default[IO].withMaxIterations(2)).run(request)
      asked  <- model.requests.map(_.size)
    } yield {
      assertEquals(result.stop, RunStop.MaxIterations)
      assertEquals(result.iterations, 2)
      assertEquals(asked, 2)
    }
  }

  test("a refusal or a token limit ends the run as data, without running tools") {
    for {
      m1      <- ScriptedModel.of[IO](Replies.stopped(StopReason.Refusal))
      refused <- m1.toolRunner(List(weather)).run(request)
      m2      <- ScriptedModel.of[IO](Replies.stopped(StopReason.MaxTokens))
      cutOff  <- m2.toolRunner(List(weather)).run(request)
    } yield {
      assertEquals(refused.stop, RunStop.Refused(None))
      assertEquals(cutOff.stop, RunStop.MaxTokens)
    }
  }

  test("pause_turn resends the conversation without adding a user message") {
    for {
      model  <- ScriptedModel.of[IO](Replies.stopped(StopReason.PauseTurn), Replies.text("done"))
      result <- model.toolRunner(List(weather)).run(request)
      sent   <- model.requests
    } yield {
      assertEquals(result.text, "done")
      assertEquals(sent(1).messages.toList.map(_.role), List(Role.User, Role.Assistant))
    }
  }

  test("events arrive in order: model deltas, tool start and finish, then the outcome") {
    for {
      model  <- ScriptedModel.of[IO](call("Brussels"), Replies.text("21 degrees."))
      events <- model.toolRunner(List(weather)).events(request).compile.toList
    } yield {
      val shape = events.collect {
        case RunEvent.TurnCompleted(i, _)                                       => s"turn$i"
        case RunEvent.ToolStarted(c)                                            => s"start:${c.name}"
        case RunEvent.ToolFinished(c, _)                                        => s"finish:${c.name}"
        case RunEvent.Model(_, StreamEvent.ContentBlockDelta(_, Delta.Text(t))) => s"text:$t"
        case RunEvent.Finished(r)                                               => s"finished:${r.stop}"
      }
      assertEquals(
        shape,
        List(
          "turn1",
          "start:get_weather",
          "finish:get_weather",
          "text:21 degrees.",
          "turn2",
          "finished:Completed"
        )
      )
    }
  }

  test("beforeRequest can edit each request") {
    val config = RunnerConfig.default[IO].withBeforeRequest((i, r) => IO.pure(r.withUserId(s"turn-$i")))
    for {
      model <- ScriptedModel.of[IO](call("x"), Replies.text("ok"))
      _     <- model.toolRunner(List(weather), config).run(request)
      sent  <- model.requests
    } yield assertEquals(sent.map(_.userId).toList, List(Some("turn-1"), Some("turn-2")))
  }

  test("a tool whose arguments cannot be a strict schema stops the run before any request") {
    val bad: Tool[IO] = Tool("bad", "Takes a bare string")((s: String) => IO.pure(s))
    for {
      model <- ScriptedModel.of[IO](Replies.text("unreachable"))
      err   <- model.toolRunner(List(bad)).run(request).attempt
      asked <- model.requests.map(_.size)
    } yield {
      assert(err.left.exists(_.isInstanceOf[AnthropicError.InvalidSchema]))
      assertEquals(asked, 0)
    }
  }

  test("cancelling a run cancels the tool that is executing") {
    TestControl.executeEmbed {
      for {
        cancelled <- Ref.of[IO, Boolean](false)
        hanging = Tool[IO, GetWeather, String]("get_weather", "Never returns")(_ =>
          IO.never[String].onCancel(cancelled.set(true))
        )
        model   <- ScriptedModel.of[IO](call("x"), Replies.text("unreachable"))
        outcome <- model.toolRunner(List(hanging)).run(request).timeout(5.seconds).attempt
        flag    <- cancelled.get
      } yield {
        assert(outcome.isLeft)
        assert(flag, "the tool's cancellation handler did not run")
      }
    }
  }

  test("ScriptedModel also backs parse") {
    for {
      model  <- ScriptedModel.of[IO](Replies.structured(Forecast("Ghent", 18)))
      parsed <- model.parse[Forecast](request)
      sent   <- model.requests
    } yield {
      assertEquals(parsed.value, Forecast("Ghent", 18))
      assert(sent.head.outputConfig.flatMap(_.format).isDefined)
    }
  }
}
