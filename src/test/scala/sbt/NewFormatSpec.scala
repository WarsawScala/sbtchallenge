package sbt

import java.io.File
import scala.io.Source

class NewFormatSpec extends AbstractSpec{
  implicit val splitter: SplitExpressions = new EvaluateConfigurationsScalania

  "New Format " should {
    "Handle lines " in {
      val rootPath = getClass.getResource("").getPath + "../new-format/"
      println(s"Reading files from: $rootPath")
      val allFiles = new File(rootPath).listFiles.map(_.getAbsolutePath).toList
      foreach(allFiles){
        path =>
          val lines = Source.fromFile(path).getLines().toList
          val (_,statements) = splitter.splitExpressions(lines)
          statements.nonEmpty must be_==(true).orPending( s"""
                       |***should contains statements***
                       |$lines """.stripMargin)
      }
    }
  }

}
