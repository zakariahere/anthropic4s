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

import anthropic4s.schema.{Schema, SchemaIssue}
import cats.MonadThrow
import cats.syntax.all._
import io.circe.Json

/** How a tool's return value is shown to the model. */
trait ToolOutput[A] {
  def content(value: A): ToolResultContent
}

object ToolOutput extends ToolOutputLowPriority {
  def apply[A](implicit out: ToolOutput[A]): ToolOutput[A] = out

  def instance[A](f: A => ToolResultContent): ToolOutput[A] = new ToolOutput[A] {
    def content(value: A): ToolResultContent = f(value)
  }

  implicit val string: ToolOutput[String]             = instance(ToolResultContent.Text(_))
  implicit val unit: ToolOutput[Unit]                 = instance(_ => ToolResultContent.Text("done"))
  implicit val json: ToolOutput[Json]                 = instance(j => ToolResultContent.Text(j.noSpaces))
  implicit val content: ToolOutput[ToolResultContent] = instance(identity)
  implicit val blocks: ToolOutput[List[ContentBlock]] = instance(ToolResultContent.Blocks(_))
}

private[anthropic4s] trait ToolOutputLowPriority {

  /**
   * Anything with a [[anthropic4s.schema.Schema]] (case classes included) is returned to the model as JSON.
   */
  implicit def fromSchema[A](implicit schema: Schema[A]): ToolOutput[A] =
    ToolOutput.instance(a => ToolResultContent.Text(schema.encoder(a).noSpaces))
}

/**
 * Something the model can call: a definition the API sees, and the code that runs when it is called.
 *
 * The usual way to make one is from a case class describing the arguments:
 * {{{
 * final case class GetWeather(@description("City name, e.g. Brussels") city: String)
 *
 * val weather = Tool("get_weather", "Current weather for a city") { (in: GetWeather) =>
 *   weatherService.lookup(in.city)          // IO[String], IO[SomeCaseClass], ...
 * }
 * }}}
 * The argument schema is derived, sent as a strict tool, and the arguments are decoded before your function
 * runs.
 */
trait Tool[F[_]] {
  def definition: ToolDefinition.Custom

  /** Runs the tool. Bad arguments come back as an error result for the model, not as a failed effect. */
  def run(call: ContentBlock.ToolUse): F[ContentBlock.ToolResult]

  /** Reasons the argument type has no strict schema. A runner refuses to start while this is non-empty. */
  def schemaIssues: List[SchemaIssue]

  final def name: String = definition.name
}

object Tool {

  def apply[F[_], In, Out](name: String, description: String)(
      f: In => F[Out]
  )(implicit F: MonadThrow[F], in: Schema[In], out: ToolOutput[Out]): Tool[F] = {
    val strict = in.strict
    // Built out here on purpose: inside the anonymous class, `name` would mean `Tool.name`, not the parameter.
    val custom = ToolDefinition.custom(name, description, in.jsonSchema).withStrict(true)
    new Tool[F] {
      val definition: ToolDefinition.Custom = custom

      val schemaIssues: List[SchemaIssue] = strict.fold(_.toList, _ => Nil)

      def run(call: ContentBlock.ToolUse): F[ContentBlock.ToolResult] =
        call.invalidInputJson match {
          case Some(_) =>
            // A streamed call cut off mid-arguments. Telling the model lets it send the call again.
            F.pure(
              ContentBlock.ToolResult
                .error(call.id, "INVALID_JSON: the tool arguments were not complete, valid JSON")
            )
          case None =>
            in.decoder.decodeJson(Json.fromJsonObject(call.input)) match {
              case Left(e) =>
                F.pure(ContentBlock.ToolResult.error(call.id, s"Invalid arguments: ${e.getMessage}"))
              case Right(input) =>
                f(input).map(result => ContentBlock.ToolResult(call.id, out.content(result)))
            }
        }
    }
  }

  /** A tool with a hand-written definition that receives the raw call. */
  def raw[F[_]](
      custom: ToolDefinition.Custom
  )(f: ContentBlock.ToolUse => F[ContentBlock.ToolResult]): Tool[F] =
    new Tool[F] {
      val definition: ToolDefinition.Custom                           = custom
      val schemaIssues: List[SchemaIssue]                             = Nil
      def run(call: ContentBlock.ToolUse): F[ContentBlock.ToolResult] = f(call)
    }
}
