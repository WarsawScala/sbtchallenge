package sbt

import java.io.File

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import scala.annotation.tailrec
import scala.io.Source
import scala.tools.reflect.ToolBoxError
import scala.util.{Failure, Success, Try}


@RunWith(classOf[JUnitRunner])
class SplitExpressionsFilesTest extends AbstractSplitExpressionsFilesTest("../old-format/")


abstract class AbstractSplitExpressionsFilesTest(pathName: String) extends FlatSpec {

  case class SplitterComparison(oldSplitterResult: Try[(Seq[(String, Int)], Seq[LineRange])], newSplitterResult: Try[(Seq[(String, Int)], Seq[LineRange])])

  val oldSplitter = new EvaluateConfigurationsOriginal
  val newSplitter = new EvaluateConfigurationsScalania

  final val REVERTED_LINES = true
  final val START_COMMENT = "/*"
  final val END_COMMENT = START_COMMENT.reverse

  it should "split whole sbt files" in {
    val rootPath = getClass.getResource("").getPath + pathName
    println(s"Reading files from: $rootPath")
    val allFiles = new File(rootPath).listFiles.map(_.getAbsolutePath).toList

    val results = for {
      path <- allFiles
      lines = Source.fromFile(path).getLines().toList
      comparison = SplitterComparison(splitLines(oldSplitter, lines), splitLines(newSplitter, lines))
    } yield path -> comparison

    printResults(results)

    val validResults = results.collect {
      case (path, SplitterComparison(Success(oldRes), Success(newRes))) if oldRes == newRes => path
    }

    assert(validResults.length === results.length, " - Errors or result differences occurred.")
  }


  def removeCommentFromStatement(statement: String, lineRange: LineRange): Option[LineRange] = {
    val lines = statement.lines.toList
    val optionStatements = removeSlashAsterisk(lines, lineRange,!REVERTED_LINES) match {
      case Some((st, lr)) =>
        removeDoubleSlash(st, lr)
      case _ => None
    }
    optionStatements.map(t => t._2)
  }


  @tailrec
  private def removeSlashAsterisk(statements: Seq[String], lineRange: LineRange,reverted:Boolean): Option[(Seq[String], LineRange)] =
    statements match {
      case statement +: _ =>
        val openSlashAsteriskIndex = statement.indexOf(START_COMMENT, 0)
        if (openSlashAsteriskIndex == -1 || statement.substring(0, openSlashAsteriskIndex).trim.nonEmpty) {
          Some((statements, lineRange))
        } else {
          val closeSlashAsteriskLine = statements.indexWhere(s => s.contains(END_COMMENT))
          if (closeSlashAsteriskLine == -1) {
            Some((statements, lineRange))
          } else {
            val newLineRange = if(reverted){
              lineRange.copy(end = lineRange.end - closeSlashAsteriskLine - 1)
            }else {
              lineRange.copy(start = lineRange.start + closeSlashAsteriskLine + 1)
            }
            removeSlashAsterisk(statements.drop(closeSlashAsteriskLine + 1), newLineRange,reverted)
          }
        }
      case _ =>
        None
    }

  /**
   * Remove // and /* */
   * @param statements - lines
   * @param lineRange - LineRange
   * @return (lines,lineRange) without comments
   */
  def removeDoubleSlash(statements: Seq[String], lineRange: LineRange): Option[(Seq[String], LineRange)] = {

    @tailrec
    def removeDoubleSlashReversed(lines: Seq[String], lineRange: LineRange): Option[(Seq[String], LineRange)] =
      lines match {
        case statement +: _ =>
          val doubleSlashIndex = statement.indexOf("//")
          if (doubleSlashIndex == -1 || statement.substring(0, doubleSlashIndex).trim.nonEmpty) {
            removeSlashAsterisk(lines, lineRange,REVERTED_LINES) match {
              case some@Some((s, ln)) if ln == lineRange =>
                some
              case Some((s, ln)) =>
                removeDoubleSlashReversed(s, ln)
              case _ => None
            }

          } else {
            removeDoubleSlashReversed(lines.tail, lineRange.copy(end = lineRange.end - 1))
          }
        case _ =>
          None
      }
    removeDoubleSlashReversed(statements.reverse, lineRange).map(t => (t._1.reverse, t._2))
  }

  def splitLines(splitter: SplitExpressions, lines: List[String]): Try[(Seq[(String, Int)], Seq[LineRange])] = {
    try {
      val (imports, settingsAndDefs) = splitter.splitExpressions(lines)

      //TODO: Return actual contents (after making both splitter...
      //TODO: ...implementations return CharRanges instead of LineRanges)
      val settingsAndDefWithoutComments = settingsAndDefs.flatMap(t => removeCommentFromStatement(t._1, t._2))
      Success((imports.map(imp => (imp._1.trim, imp._2)), settingsAndDefWithoutComments))
    }
    catch {
      case e: ToolBoxError =>
        Failure(e)
      case e: Throwable =>
        Failure(e)
    }
  }

  def printResults(results: List[(String, SplitterComparison)]) = {
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