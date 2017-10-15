name := "regextools"
version := "0.0.1-SNAPSHOT"
organization := "ai.lum"
scalaVersion := "2.11.11"
crossScalaVersions := Seq("2.11.11", "2.12.3")
scalacOptions := Seq(
  "-encoding", "utf8",
  "-feature",
  "-unchecked",
  "-deprecation",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Ywarn-unused"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.4" % "test"
)
