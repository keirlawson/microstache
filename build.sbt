import Dependencies._

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "microstache",
    libraryDependencies ++= List(
      "org.typelevel" %% "cats-effect" % "3.5.1",
      "org.typelevel" %% "cats-parse" % "0.3.10",
      "io.circe" %% "circe-core" % "0.14.5",
      munit % Test
    ),
    scalacOptions ++= List(
      "-deprecation",
    )
  )

