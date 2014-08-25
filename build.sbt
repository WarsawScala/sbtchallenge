name := "sbtchallenge"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % "2.10.4",
  "org.scalatest" %% "scalatest" % "2.2.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
  "junit" % "junit" % "4.10" % "test"
)

instrumentSettings

CoverallsPlugin.coverallsSettings

EclipseKeys.withSource := true

EclipseKeys.withBundledScalaContainers := false
