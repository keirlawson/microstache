val munitVersion = "0.7.29"

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "microstache"
  ).aggregate(core, circe)

lazy val core = project.settings(
    name := "microstache",
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-parse" % "0.3.10",
      "org.scalameta" %% "munit" % munitVersion % Test,
      "org.scalameta" %% "munit-scalacheck" % munitVersion % Test
    ),
)

lazy val circe = project.settings(
  name := "microstache-circe",
  libraryDependencies ++= List(
    "io.circe" %% "circe-core" % "0.14.5",
  ),
).dependsOn(core  % "test->test;compile->compile")