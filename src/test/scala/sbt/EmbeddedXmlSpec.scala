package sbt

import org.scalatest.prop.Checkers


class EmbeddedXmlSpec extends AbstractSpec with Checkers {
  implicit val splitter = new EvaluateConfigurationsScalania

  "File with xml content " should {
    "Handle xml part" in {
      val buildSbt = s"""

name := "play-html-compressor"

scalaVersion := "2.11.1"

val ok = <ccc atr="tre" />

val pom = <scm>
    <url>git@github.com:mohiva/play-html-compressor.git</url>
    <connection>scm:git:git@github.com:mohiva/play-html-compressor.git</connection>
  </scm>
  <developers>
    <developer>
      <id>akkie</id>
      <name>Christian Kaps</name>
      <url>http://mohiva.com</url>
    </developer>
  </developers>
  <version>4.0</version>

publishMavenStyle := true

val anotherXml = <a a="r"><bbb>
        content</bbb>
        <ccc atr="tre" />
        <aa/>
          </a>

"""

      split(buildSbt)

    }

    "Handle last xml part" in {
      val errorLine = """<version>4.0<version>"""
      val buildSbt = s"""

name := "play-html-compressor"

scalaVersion := "2.11.1"

val pom = <scm>
    <url>git@github.com:mohiva/play-html-compressor.git</url>
    <connection>scm:git:git@github.com:mohiva/play-html-compressor.git</connection>
  </scm>
  <developers>
    <developer>
      <id>akkie</id>
      <name>Christian Kaps</name>
      <url>http://mohiva.com</url>
    </developer>
  </developers>
  $errorLine

"""
      val exception = intercept[MessageOnlyException] {
        split(buildSbt)
      }
      val index = buildSbt.lines.indexWhere(line => line.contains(errorLine))
      exception.getMessage.matches(s".*$index.*")


    }


    "Xml in string" in {
      val buildSbt = s"""

name := "play-html-compressor"

scalaVersion := "2.11.1"

val lll = "</=+=>"

val not = "<sss><sss>"


val pom = "</scm>"

val aaa= <scm><url>git@github.com:mohiva/play-html-compressor.git</url>
    <connection>scm:git:git@github.com:mohiva/play-html-compressor.git</connection>
  </scm>
  <developers>
    <developer>
      <id>akkie</id>
      <name>Christian Kaps</name>
      <url>http://mohiva.com</url>
    </developer>
  </developers>
  <version>4.0</version>

publishMavenStyle := true

val anotherXml = <a a="r"><bbb>
        content</bbb>
        <ccc atr="tre" />
        <aa/>
          </a>

"""

      split(buildSbt)

    }

  }

}
