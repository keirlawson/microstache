import xerial.sbt.Sonatype._

val munitVersion = "0.7.29"

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "io.github.keirlawson"
ThisBuild / sonatypeCredentialHost := "s01.oss.sonatype.org"
ThisBuild / versionScheme     := Some("semver-spec")

lazy val root = (project in file("."))
  .settings(
    name := "microstache",
    publish / skip := true,
  ).aggregate(core, circe)

lazy val commonSettings = Seq(
    publishTo := sonatypePublishToBundle.value,
    sonatypeProfileName := "io.github.keirlawson",
    publishMavenStyle := true,
    licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
    sonatypeProjectHosting := Some(GitHubHosting("keirlawson", "microstache", "keirlawson@gmail.com"))
)

lazy val core = project.settings(
    name := "microstache",
    commonSettings,
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-parse" % "0.3.10",
      "org.scalameta" %% "munit" % munitVersion % Test,
      "org.scalameta" %% "munit-scalacheck" % munitVersion % Test
    ),
)

lazy val circe = project.settings(
  name := "microstache-circe",
  commonSettings,
  libraryDependencies ++= List(
    "io.circe" %% "circe-core" % "0.14.5",
  ),
).dependsOn(core  % "test->test;compile->compile")