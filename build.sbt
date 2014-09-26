name := "sbtchallenge"

version := "1.0"

scalaVersion := "2.10.4"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature","-Yrangepos")

libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scalacheck" %% "scalacheck" % "1.11.5" % "test",
  "org.specs2" %% "specs2" % "2.4.2" % "test",
  "junit" % "junit" % "4.11" % "test"
)

instrumentSettings

CoverallsPlugin.coverallsSettings

EclipseKeys.withSource := true

EclipseKeys.withBundledScalaContainers := false
