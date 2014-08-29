package sbt

import java.io.File

import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.specs2.ScalaCheck

class ErrorSpec extends AbstractSpec with ScalaCheck {
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

    "Contains file name " in {
      val file = new File("target/aa")
      val buildSbt =
        s"""
         | val a = "se
       """.stripMargin

      SplitExpressionsNoBlankies(file, buildSbt.lines.toSeq) must throwA[MessageOnlyException].like {
        case exp =>
          exp.getMessage must contain(file.getName)
      }
    }

    "Bug in parser " in {
      val buildSbt =
        """
          |libraryDependencies ++= Seq("a" % "b" % "2") map {
          |(dependency) =>{
          | dependency
          | } /* */ //
          |}
        """.stripMargin
      BugInParser.tryWithNextStatement(buildSbt, "", buildSbt.length, 2, "fake.txt", new MessageOnlyException("fake")) must throwA[MessageOnlyException]
    }
  }


  private def containsLineNumber(buildSbt: String) = {
    try {
      split(buildSbt)
      throw new IllegalStateException(s"${classOf[MessageOnlyException].getName} expected")
    } catch {
      case exception: MessageOnlyException =>
        val error = exception.getMessage
        """(\d+)""".r.findFirstIn(error) match {
          case Some(x) =>
            true
          case None =>
            println(s"Number not found in $error")
            false
        }
    }
  }
}
