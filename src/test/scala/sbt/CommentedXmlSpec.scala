package sbt

import org.scalatest.prop.Checkers


class CommentedXmlSpec extends AbstractSpec with Checkers {
  implicit val splitter = new EvaluateConfigurationsScalania

  "Commented part of code " should {
    "Xml in string" in {
      val buildSbt = s"""

val pom = "</scm>"

val aaa= <scm><url>git@github.com:mohiva/play.git</url>
    <cc>ewrer</cc>
  </scm>

val tra = "</scm>"

"""

      split(buildSbt)

    }

    "Xml in comment" in {
      val buildSbt =
        """
          |// a/>
        """.stripMargin

      split(buildSbt)
    }

    "Xml in comments" in {
      val buildSbt =
        """
          |import sbt._
          |
          |// </a
        """.stripMargin

      split(buildSbt)
    }

    "Commented xml " in {
      val buildSbt =
        """
          |val scmpom = taskKey[xml.NodeBuffer]("Node buffer")
          |
          |scmpom := <scm>
          |    <url>git@github.com:mohiva/play-html-compressor.git</url>
          |    <connection>scm:git:git@github.com:mohiva/play-html-compressor.git</connection>
          |  </scm>
          | <developers>
          |    <developer>
          |      <id>akkie</id>
          |      <name>Christian Kaps</name>
          |      <url>http://mohiva.com</url>
          |    </developer>
          |  </developers>
          |  //<aaa/>
          |  //<a></a>
          |
          |publishMavenStyle := true
          |
        """.stripMargin
      split(buildSbt)
    }

    "Wrong Commented xml " in {
      val buildSbt =
        """
          |val scmpom = taskKey[xml.NodeBuffer]("Node buffer")
          |
          |scmpom := <scm>
          |    <url>git@github.com:mohiva/play-html-compressor.git</url>
          |    <connection>scm:git:git@github.com:mohiva/play-html-compressor.git</connection>
          |  </scm>
          | <developers>
          |    <developer>
          |      <id>akkie</id>
          |      <name>Christian Kaps</name>
          |      <url>http://mohiva.com</url>
          |    </developer>
          |  </developers>
          |  //<aaa/>
          |  <a></a>
          |
          |publishMavenStyle := true
          |
        """.stripMargin
      split(buildSbt)
    }
  }
}
