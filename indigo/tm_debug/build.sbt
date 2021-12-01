Test / scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }

ThisBuild / scalaVersion := "3.1.0"

name := "testgame.tilemaps"

lazy val mygame =
  project.in(file("."))
    .enablePlugins(ScalaJSPlugin, SbtIndigo)
    .settings( // Normal SBT settings
      name := "perf",
      version := "0.0.1",
      scalaVersion := "3.1.0",
      organization := "indigo",
      libraryDependencies ++= Seq(
        "com.lihaoyi"    %%% "utest"      % "0.7.10"  % "test",
        "org.scalacheck" %%% "scalacheck" % "1.15.4" % "test"
      ),
      testFrameworks += new TestFramework("utest.runner.Framework")
    )
    .settings( // Indigo specific settings
      showCursor := true,
      title := "Test - Tilemaps",
      gameAssetsDirectory := "assets",
      windowStartWidth := 1152,
      windowStartHeight := 720,
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "indigo-json-circe" % "0.10.1-SNAPSHOT",
        "io.indigoengine" %%% "indigo"            % "0.10.1-SNAPSHOT",
        "io.indigoengine" %%% "indigo-extras"     % "0.10.1-SNAPSHOT",
        "com.github.plokhotnyuk.rtree2d" %%% "rtree2d-core" % "0.11.9"
      )
    )

addCommandAlias("buildGame", ";compile;fastOptJS;indigoBuild")
addCommandAlias("runGame", ";compile;fastOptJS;indigoRun")
addCommandAlias("buildGameFull", ";compile;fullOptJS;indigoBuildFull")
addCommandAlias("runGameFull", ";compile;fullOptJS;indigoRunFull")
addCommandAlias("buildGameFullCordova", ";compile;fullOptJS;indigoBuildFull;indigoCordovaBuildFull")

resolvers += Resolver.jcenterRepo
