package sbt

import java.io.File

import org.scalatest.FlatSpec

import scala.io.Source
import scala.util.{Failure, Success, Try}

class SplitExpressionsMyTest extends FlatSpec {

  case class SplitterComparison(oldSplitterResult: Try[(Seq[Int], Seq[LineRange])], newSplitterResult: Try[(Seq[Int], Seq[LineRange])])

  val oldSplitter = new EvaluateConfigurationsOriginal
  val newSplitter = new EvaluateConfigurationsScalania

  it should "split whole sbt files" in {
    val rootPath = getClass.getResource("").getPath + "../old-format/"
    println(s"Reading files from: $rootPath")
    val results = for {
      path <- new File(rootPath).listFiles.filter(f => f.getName == "20.sbt.txt").map(_.getAbsolutePath).toSeq
      lines = Source.fromFile(path).getLines().toList
      comparison = SplitterComparison(splitLines(oldSplitter, lines), splitLines(newSplitter, lines))
    } yield path -> comparison

    printResults(results)

    val validResults = results.collect {
      case (path, SplitterComparison(Success(oldRes), Success(newRes))) if oldRes == newRes => path
    }

    assert(validResults.length === results.length, " - Errors or result differences occurred.")
  }


  def splitLines(splitter: SplitExpressions, lines: List[String]): Try[(Seq[Int], Seq[sbt.LineRange])] =
    Try(splitter.splitExpressions(lines)).map {
      case (imports, settingsAndDefs) => (imports.map(_._2), settingsAndDefs.map(_._2))
    }.recover { case (exp: Throwable) =>
      exp.printStackTrace()
      throw exp
    }


  def printResults(results: Seq[(String, SplitterComparison)]) = {
    for ((path, comparison) <- results) {
      val fileName = new File(path).getName
      comparison match {
        case SplitterComparison(Failure(ex), _) =>
          println(s"In file: $fileName, old splitter failed. ${ex.toString}")
        case SplitterComparison(_, Failure(ex)) =>
          println(s"In file: $fileName, new splitter failed. ${ex.toString}")
        case SplitterComparison(Success(resultOld), Success(resultNew)) =>
          if (resultOld == resultNew) {
            println(s"In file: $fileName, same results (imports, settings): $resultOld")
          } else {
            println(
              s"""In file: $fileName, results differ:
                 |resultOld:
                 |$resultOld
                 |resultNew:
                 |$resultNew""".stripMargin)
          }
      }

    }
  }
}