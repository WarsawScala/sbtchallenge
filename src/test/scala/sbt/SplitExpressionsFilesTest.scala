package sbt

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FlatSpec
import scala.io.Source
import java.io.{FileInputStream, FileOutputStream, File}
import scala.util.{Failure, Success, Try}
import scala.tools.reflect.ToolBoxError

@RunWith(classOf[JUnitRunner])
class SplitExpressionsFilesTest extends FlatSpec {

   case class SplitterComparison(oldSplitterResult: Try[(Seq[(String,Int)], Seq[LineRange])], newSplitterResult: Try[(Seq[(String,Int)], Seq[LineRange])])

   val oldSplitter = new EvaluateConfigurationsOriginal
   val newSplitter = new EvaluateConfigurationsScalania

   it should "split whole sbt files" in {
      val rootPath = getClass.getResource("").getPath + "../old-format/"
      println(s"Reading files from: $rootPath")
      val allFiles = new File(rootPath).listFiles.map(_.getAbsolutePath).toList

      val results = for {
         path <- allFiles
         lines = Source.fromFile(path).getLines().toList
         comparison = SplitterComparison(splitLines(oldSplitter, lines), splitLines(newSplitter, lines))
      } yield path -> comparison

      printResults(results)

      val validResults = results.collect{
         case (path, SplitterComparison(Success(oldRes), Success(newRes))) if oldRes == newRes => path
      }

      assert(validResults.length === results.length, " - Errors or result differences occurred.")
   }


   def splitLines(splitter: SplitExpressions, lines: List[String]) = {
      try {
         val (imports, settingsAndDefs) = splitter.splitExpressions(lines)

         //TODO: Return actual contents (after making both splitter...
         //TODO: ...implementations return CharRanges instead of LineRanges)
         Success((imports.map(imp => (imp._1.trim,imp._2)), settingsAndDefs.map(_._2)))
      }
      catch {
         case e:ToolBoxError =>
            Failure(e)
         case e:Throwable =>
            Failure(e)
      }
   }

   def printResults(results: List[(String, SplitterComparison)]) = {
      for((path, comparison) <- results) {
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