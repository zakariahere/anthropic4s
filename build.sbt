// anthropic4s — an unofficial, pure-Scala SDK for the Claude API.

ThisBuild / tlBaseVersion       := "0.1"
ThisBuild / organization        := "io.github.zakariahere"
ThisBuild / organizationName    := "anthropic4s contributors"
ThisBuild / startYear           := Some(2026)
ThisBuild / licenses            := Seq(License.Apache2)
ThisBuild / developers          := List(tlGitHubDev("zakariahere", "Zakaria"))
ThisBuild / tlCiReleaseBranches := Seq.empty // nothing is published automatically

val Scala213 = "2.13.18"
val Scala3   = "3.3.6" // the LTS line; only `model` is cross-built for now
ThisBuild / scalaVersion               := Scala213
ThisBuild / crossScalaVersions         := Seq(Scala213)
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("17"), JavaSpec.temurin("21"))

// The wire model has no macros and must stay source-compatible with Scala 3, so that cross-building the
// rest later is a matter of adding the derivation glue, not of rewriting types.
ThisBuild / githubWorkflowBuild +=
  WorkflowStep.Sbt(List("+model/Test/compile"), name = Some("Compile the wire model on Scala 3"))

val V = new {
  val cats            = "2.13.0"
  val catsEffect      = "3.7.1"
  val fs2             = "3.16.0"
  val http4s          = "0.23.39"
  val circe           = "0.14.17"
  val magnolia        = "1.1.14"
  val otel4s          = "1.2.0"
  val munit           = "1.3.6"
  val munitCatsEffect = "2.2.1"
  val playJson        = "3.0.6"
}

// `-Xsource:3-cross` makes a case class with a private constructor get a private `apply` and `copy` too.
// Every SDK data type relies on that: new API fields are added through `withX` methods, which keeps
// already-compiled user code binary compatible.
val commonSettings = Seq(
  scalacOptions ++= {
    if (scalaBinaryVersion.value == "2.13") Seq("-Xsource:3-cross") else Seq.empty
  },
  libraryDependencies += "org.scalameta" %% "munit" % V.munit % Test
)

lazy val root = project
  .in(file("."))
  .enablePlugins(NoPublishPlugin)
  .aggregate(model, schema, core, ember, playJson, testkit, otel, examples)

lazy val schema = project
  .in(file("schema"))
  .settings(commonSettings)
  .settings(
    name := "anthropic4s-schema",
    libraryDependencies ++= Seq(
      "org.typelevel"                %% "cats-core"     % V.cats,
      "io.circe"                     %% "circe-core"    % V.circe,
      "io.circe"                     %% "circe-parser"  % V.circe            % Test,
      "com.softwaremill.magnolia1_2" %% "magnolia"      % V.magnolia,
      "org.scala-lang"                % "scala-reflect" % scalaVersion.value % Provided
    )
  )

lazy val core = project
  .in(file("core"))
  .dependsOn(model, schema)
  .settings(commonSettings)
  .settings(
    name := "anthropic4s-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"         % V.catsEffect,
      "co.fs2"        %% "fs2-core"            % V.fs2,
      "org.http4s"    %% "http4s-client"       % V.http4s,
      "org.http4s"    %% "http4s-circe"        % V.http4s,
      "org.typelevel" %% "munit-cats-effect"   % V.munitCatsEffect % Test,
      "org.typelevel" %% "cats-effect-testkit" % V.catsEffect      % Test,
      "org.http4s"    %% "http4s-dsl"          % V.http4s          % Test
    )
  )

// The artifact most users depend on: core plus a ready-to-use HTTP client.
lazy val ember = project
  .in(file("ember"))
  .dependsOn(core, testkit % Test)
  .settings(commonSettings)
  .settings(
    name := "anthropic4s",
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-ember-client" % V.http4s,
      "co.fs2"        %% "fs2-io"              % V.fs2, // keeps fs2-io in step with fs2-core
      "org.http4s"    %% "http4s-ember-server" % V.http4s          % Test,
      "org.http4s"    %% "http4s-dsl"          % V.http4s          % Test,
      "org.typelevel" %% "munit-cats-effect"   % V.munitCatsEffect % Test
    )
  )

lazy val model = project
  .in(file("model"))
  .settings(commonSettings)
  .settings(
    name               := "anthropic4s-model",
    crossScalaVersions := Seq(Scala213, Scala3),
    // Without this, the plugin picks the module default from the list above and lands on Scala 3.
    scalaVersion := Scala213,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"    % V.cats,
      "io.circe"      %% "circe-core"   % V.circe,
      "io.circe"      %% "circe-parser" % V.circe
    )
  )

// Structured outputs and tool inputs for people who already have Play JSON `Format`s.
lazy val playJson = project
  .in(file("play-json"))
  .dependsOn(schema)
  .settings(commonSettings)
  .settings(
    name := "anthropic4s-play-json",
    libraryDependencies ++= Seq(
      "org.playframework" %% "play-json"     % V.playJson,
      "io.circe"          %% "circe-parser"  % V.circe            % Test,
      "org.scala-lang"     % "scala-reflect" % scalaVersion.value % Provided
    )
  )

// Fakes and builders for testing code that uses the SDK, without a network.
lazy val testkit = project
  .in(file("testkit"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "anthropic4s-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "munit-cats-effect"   % V.munitCatsEffect % Test,
      "org.typelevel" %% "cats-effect-testkit" % V.catsEffect      % Test
    )
  )

// OpenTelemetry tracing. Depends only on the otel4s API; the application chooses the backend.
lazy val otel = project
  .in(file("otel4s"))
  .dependsOn(core, testkit % Test)
  .settings(commonSettings)
  .settings(
    name := "anthropic4s-otel4s",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "otel4s-core-trace"             % V.otel4s,
      "org.typelevel" %% "otel4s-oteljava-trace-testkit" % V.otel4s          % Test,
      "org.typelevel" %% "munit-cats-effect"             % V.munitCatsEffect % Test,
      "org.http4s"    %% "http4s-client"                 % V.http4s          % Test
    )
  )

// Runnable examples. They are compiled with the build, which keeps the documentation honest.
lazy val examples = project
  .in(file("examples"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(ember, playJson, otel)
  .settings(commonSettings)
  .settings(
    name := "anthropic4s-examples",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "otel4s-oteljava" % V.otel4s,
      // The Traced example sends spans over OTLP (to localhost:4318 unless OTEL_EXPORTER_OTLP_ENDPOINT says otherwise).
      "io.opentelemetry" % "opentelemetry-exporter-otlp" % "1.68.0",
      "org.slf4j" % "slf4j-nop" % "1.7.36" // the examples do not log; this keeps SLF4J quiet
    ),
    // A fresh JVM per run, as IOApp expects. It inherits the environment of the sbt server.
    run / fork := true,
    // Windows consoles default to a legacy code page; without this, characters such as the degree sign are mangled.
    run / javaOptions ++= Seq(
      "-Dfile.encoding=UTF-8",
      "-Dstdout.encoding=UTF-8",
      "-Dstderr.encoding=UTF-8",
      "-Dotel.service.name=anthropic4s-examples"
    )
  )
