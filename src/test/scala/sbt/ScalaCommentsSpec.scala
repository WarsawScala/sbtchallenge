package sbt

import java.io.File

import org.specs2.mutable.Specification

import scala.io.Source

class ScalaCommentsSpec extends Specification {

  private val pathName = "../tokenize-parser"

  s"$getClass " should {
    "Return the same statement as original " in {
      val rootPath = getClass.getResource("").getPath + pathName
      println(s"Reading files from: $rootPath")
      val allFiles = new File(rootPath).listFiles
      foreach(allFiles) {
        file =>
          println(s"Reading file: $file")
          val lines = Source.fromFile(file).getLines().toList
          val content = lines.mkString("\n").trim
          SplitExpressionsNoBlankies(file,lines)
          val from = content.substring(0, content.size - 1).lastIndexOf("}")
          val index = BugInParser.findFirstNotBlankNotCommentedIndex(content, from + 1)
          content should_== content.substring(0, index.get + 1)
      }

    }

  }

}
