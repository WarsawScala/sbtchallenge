package sbt

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.specs2.ScalaCheck

class ErrorSpec extends AbstractSpec with ScalaCheck  {
  implicit val splitter = new EvaluateConfigurationsScalania

  "Errors " should {
    "Show error line number" in {

      check(forAll(alphaStr) {
        (errorText) =>
          errorText.nonEmpty ==> {
            val buildSbt = s"""import sbt._
                       |import aaa._
                       |
                       |import scala._
                       |
                       |scalaVersion in Global := "2.11.2"
                       |
                       |ala
                       |
                       |libraryDependencies in Global ++= Seq(
                       |  $errorText
                       |  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
                       |  "ch.qos.logback" % "logback-classic" % "1.1.2"
                       |)""".stripMargin


            containsLineNumber(buildSbt)
          }
      })
    }

    "Contain line number" in {
      val buildSbt =
        """
          | val a = "ss
        """.stripMargin
      containsLineNumber(buildSbt)
    }
  }


  private def containsLineNumber(buildSbt: String) = {
    try {
      split(buildSbt)
      throw new IllegalStateException()
    } catch {
      case exception: MessageOnlyException =>
        val error = exception.getMessage
        """(\d+)""".r.findFirstIn(error) match {
          case Some(x) =>
            true
          case None => println(s"Number not found in $error")
            false
        }
    }
  }
}
