name := "sbtchallenge"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
  "org.specs2" %% "specs2" % "2.4.1" % "test",
  "junit" % "junit" % "4.11" % "test"
)

instrumentSettings

CoverallsPlugin.coverallsSettings

EclipseKeys.withSource := true

EclipseKeys.withBundledScalaContainers := false
