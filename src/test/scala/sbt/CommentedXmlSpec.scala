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
  }
}
