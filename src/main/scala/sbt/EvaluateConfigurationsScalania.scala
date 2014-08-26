package sbt

import scala.util.{Failure, Success, Try}

class EvaluateConfigurationsScalania extends SplitExpressions {
  def splitExpressions(lines: Seq[String]): (Seq[(String, Int)], Seq[(String, LineRange)]) = {
    import scala.reflect.runtime._
    import scala.reflect.runtime.universe._
    import scala.tools.reflect.ToolBox
    import scala.tools.reflect.ToolBoxError
    import scala.compat.Platform.EOL

    val mirror = universe.runtimeMirror(this.getClass.getClassLoader)
    val toolbox = mirror.mkToolBox(options = "-Yrangepos")
    val original = lines.mkString("\n")
    val merged = handleXmlContent(original)
    // block try-catch should be added in private[sbt] def evaluateSbtFile.
    // Here we do not have information about input file
    val parsed =
      try {
        toolbox.parse(merged)
      } catch {
        case e: ToolBoxError =>
          val seq = toolbox.frontEnd.infos.map(i =>
            s"""[${i.severity}]: [Here should be file name]:${i.pos.line}: ${i.msg}"""
          )
          throw new MessageOnlyException(
            s"""$merged
               |
               |${seq.mkString(EOL)}""".stripMargin)
      }
    val parsedTrees = parsed match {
      case Block(stmt, expr) =>
        stmt :+ expr
      case t: Tree =>
        Seq(t)
    }

    val (imports, statements) = parsedTrees partition {
      case _: Import => true
      case _ => false
    }

    def convertImport(t: Tree): (String, Int) =
      (merged.substring(t.pos.start, t.pos.end), t.pos.line)

    def convertStatement(t: Tree): Option[(String, LineRange)] = if (t.pos.isDefined) {
      Some((merged.substring(t.pos.start, t.pos.end), LineRange(t.pos.line, t.pos.end)))
    } else {
      None
    }

    (imports map convertImport, (statements map convertStatement).flatten)
  }

  private[sbt] def handleXmlContent(original: String): String = {
    val xmlParts = findXmlParts(original)
    if (xmlParts.isEmpty) {
      original
    } else {
      //println( s"""${xmlParts.mkString("\n")}""")
      addExplicitXmlContent(original, xmlParts)
    }
  }

  private def removeEmbeddedXmlParts(xmlParts: Seq[(String, Int, Int)]) = {
    def elementBetween(el: (String, Int, Int), open: Int, close: Int): Boolean = {
      xmlParts.exists {
        element =>
          val (_, openIndex, closeIndex) = element
          el != element && (open > openIndex) && (close < closeIndex)
      }
    }
    xmlParts.filterNot { el =>
      val (_, open, close) = el
      elementBetween(el, open, close)
    }
  }

  private def addExplicitXmlContent(str: String, to: Seq[(String, Int, Int)]): String = {
    val all: Seq[(String, Boolean)] = splitFile(str, to)
    val builder = new StringBuilder
    val (wasPreviousXml, wasXml) = all.foldLeft((false, false)) {
      (acc, el) =>
        val (wasXml, _) = acc
        val (content, isXml) = el
        val contentEmpty = content.trim.isEmpty
        if (isXml) {
          if (!wasXml) {
            builder.append(" ( ")
          }
        } else if (wasXml && !contentEmpty) {
          builder.append(" ) ")
        }
        builder.append(content)
        (isXml || (wasXml && contentEmpty), isXml)
    }
    if (wasPreviousXml && !wasXml) {
      builder.append(" ) ")
    }
    builder.toString()
  }

  private def splitFile(str: String, to: Seq[(String, Int, Int)]): Seq[(String, Boolean)] = {
    val (split, index) = to.foldLeft((Seq.empty[(String, Boolean)], 0)) {
      (acc, el) =>
        val (content, b, e) = el
        val (accSeq, index) = acc
        val toAdd = if (index >= b) {
          Seq((content, true))
        } else {
          val s = str.substring(index, b)
          Seq((content, true), (s, false))
        }
        (toAdd ++ accSeq, e)
    }
    ((str.substring(index, str.length), false) +: split).reverse
  }

  private def findXmlParts(content: String) = {
    val xmlParts = findModifiedOpeningTags(content, 0, Seq.empty) ++ findNotModifiedOpeningTags(content, 0, Seq.empty)
    val rootXmlParts = removeEmbeddedXmlParts(xmlParts)
    rootXmlParts.sortBy(z => z._2)

  }

  private def searchForTagName(text: String, startIndex: Int, endIndex: Int) = {
    val subs = text.substring(startIndex, endIndex)
    val spaceIndex = subs.indexOf(' ', 1)
    if (spaceIndex == -1) {
      subs
    } else {
      subs.substring(0, spaceIndex)
    }
  }

  /**
   * Modified Opening Tag - <aaa/>
   * @param offsetIndex - index
   * @param acc - result
   * @return Set with tags and positions
   */
  private def findModifiedOpeningTags(content: String, offsetIndex: Int, acc: Seq[(String, Int, Int)]): Seq[(String, Int, Int)] = {
    val endIndex = content.indexOf("/>", offsetIndex)
    if (endIndex == -1) {
      acc
    } else {
      val startIndex = content.substring(offsetIndex, endIndex).lastIndexOf("<")
      if (startIndex == -1) {
        findModifiedOpeningTags(content, endIndex + 2, acc)
      } else {
        val tagName = searchForTagName(content, startIndex + 1 + offsetIndex, endIndex)
        if (xml.Utility.isName(tagName)) {
          val xmlFragment = xmlFragmentSeq(content, startIndex + offsetIndex, endIndex + 2)
          findModifiedOpeningTags(content, endIndex + 2, xmlFragment ++ acc)
        } else {
          findModifiedOpeningTags(content, endIndex + 2, acc)
        }
      }
    }
  }

  private def searchForOpeningIndex(text: String, closeTagStartIndex: Int, tagName: String) = {
    val subs = text.substring(0, closeTagStartIndex)
    val index = subs.lastIndexOf(s"<$tagName>")
    if (index == -1) {
      subs.lastIndexOf(s"<$tagName ")
    } else {
      index
    }
  }

  /**
   * Xml like - <aaa>...<aaa/>
   * @param current - index
   * @param acc - result
   * @return Set with tags and positions
   */
  private def findNotModifiedOpeningTags(content: String, current: Int, acc: Seq[(String, Int, Int)]): Seq[(String, Int, Int)] = {
    val closeTagStartIndex = content.indexOf("</", current)
    if (closeTagStartIndex == -1) {
      acc
    } else {
      val closeTagEndIndex = content.indexOf(">", closeTagStartIndex)
      if (closeTagEndIndex == -1) {
        findNotModifiedOpeningTags(content, closeTagStartIndex + 2, acc)
      } else {
        val tagName = content.substring(closeTagStartIndex + 2, closeTagEndIndex)
        val xmlFragment = if (xml.Utility.isName(tagName)) {
          val openTagIndex = searchForOpeningIndex(content, closeTagStartIndex, tagName)
          if (openTagIndex == -1) {
            Seq.empty
          } else {
            xmlFragmentSeq(content, openTagIndex, closeTagEndIndex + 1)
          }
        } else {
          Seq.empty
        }
        findNotModifiedOpeningTags(content, closeTagEndIndex + 1, xmlFragment ++ acc)
      }
    }
  }

  private def xmlFragmentSeq(content: String, openIndex: Int, closeIndex: Int): Seq[(String, Int, Int)] = {
    val xmlPart = content.substring(openIndex, closeIndex)
    Try(xml.XML.loadString(xmlPart)) match {
      case Success(_) => Seq((xmlPart, openIndex, closeIndex))
      case Failure(th) => Seq.empty
    }
  }

}