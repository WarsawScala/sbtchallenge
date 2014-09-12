package sbt

import java.io.{File, FilenameFilter}

import org.specs2.matcher.MatchResult

import scala.collection.GenTraversableOnce
import scala.io.Source
import scala.xml.XML


class SessionSettingsSpec extends AbstractSpec {

  val rootPath = getClass.getResource("").getPath + "../session-settings"
  println(s"Reading files from: $rootPath")
  val rootDir = new File(rootPath)

  "SessionSettings " should {
    "Be identical for empty map " in {
      def unit(f: File) = Seq((Source.fromFile(f).getLines.toSeq,Map.empty[Int, (Int, List[String])]))
      runTestOnFiles(unit)
    }

    "Replace statements " in {
      def replace(f: File) = {
        val dirs = rootDir.listFiles(new FilenameFilter() {
          def accept(dir: File, name: String) ={
            val startsWith =f.getName + "_"
            name.startsWith(startsWith)
          }
        }).toList
        dirs.flatMap {
          dir =>
            val files = dir.listFiles(new FilenameFilter {
              override def accept(dir: File, name: String) = name.endsWith(".xml")
            })
            files.map { xmlFile =>
              val xml = XML.loadFile(xmlFile)
              val result = Source.fromFile(xmlFile.getAbsolutePath+".result").getLines.toSeq
              val map = (xml \\ "settings" \\ "setting").map{
                node =>
                  val set = (node \\ "set").text
                  val start = (node \\ "start").text.toInt
                  val end = (node \\ "end").text.toInt
                  (start,(end,List(set)))
              }.toMap
              (result,map)
            }
        }
      }
      runTestOnFiles(replace)
    }
  }

  private def runTestOnFiles(expectedResultAndMap: File => Seq[(Seq[String],Map[Int, (Int, List[String])])]): MatchResult[GenTraversableOnce[File]] = {

    val allFiles = rootDir.listFiles(new FilenameFilter() {
      def accept(dir: File, name: String) = name.endsWith(".sbt.txt")
    }).toList
    foreach(allFiles) {
      file =>
        val originalLines = Source.fromFile(file).getLines().toList
        val oldContentWithIndex = originalLines.zipWithIndex
        foreach(expectedResultAndMap(file)) {
          case (expectedResult,map) =>
            val result = SessionSettings.newLines(oldContentWithIndex, map)
            expectedResult === result
        }
    }
  }
}
