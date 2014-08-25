package sbt

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
            s"""${i.msg} line: ${i.pos.line}"""
          )
          throw new MessageOnlyException(seq.mkString(EOL))
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
      (merged.substring(t.pos.start, t.pos.end), t.pos.start)
    def convertStatement(t: Tree): (String, LineRange) =
      (merged.substring(t.pos.start, t.pos.end), LineRange(t.pos.start, t.pos.end))

    (imports map convertImport, statements map convertStatement)
  }

  private[sbt] def handleXmlContent(original: String): String = {
    val xmlParts = findXmlParts(original)
    if (xmlParts.isEmpty) {
      original
    } else {
      val rootXmlParts = removeEmbeddedXmlParts(xmlParts)
      val sortedXmlParts = rootXmlParts.sortBy(z => z._2)
      addExplicitXmlContent(original, sortedXmlParts)
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
        val toAdd = if (index == b) {
          Seq((content, true))
        } else {
          val s = str.substring(index, b)
          Seq((content, true), (s, false))
        }
        (toAdd ++ accSeq, e)
    }
    ((str.substring(index, str.length), false) +: split).reverse
  }

  private def findXmlParts(str: String) = {
    /**
     * Xml like - <aaa>...<aaa/>
     * @param current - index
     * @param acc - result
     * @return Set with tags and positions
     */
    def findXmlParts(current: Int, acc: Seq[(String, Int, Int)]): Seq[(String, Int, Int)] = {
      val closeTagStartIndex = str.indexOf("</", current)
      if (closeTagStartIndex == -1) {
        acc
      } else {
        val closeTagEndIndex = str.indexOf(">", closeTagStartIndex)
        if (closeTagEndIndex == -1) {
          findXmlParts(closeTagEndIndex, acc)
        } else {
          val tagName = str.substring(closeTagStartIndex + 2, closeTagEndIndex)
          if (xml.Utility.isName(tagName)) {
            val openTagIndex = searchForOpeningIndex(closeTagStartIndex, tagName)
            if (openTagIndex == -1) {
              findXmlParts(closeTagEndIndex, acc)
            } else {
              val xmlPart = (str.substring(openTagIndex, closeTagEndIndex + 1), openTagIndex, closeTagEndIndex + 1)
              findXmlParts(closeTagEndIndex, xmlPart +: acc)
            }
          } else {
            findXmlParts(closeTagEndIndex, acc)
          }
        }
      }
    }

    def searchForOpeningIndex(closeTagStartIndex: Int, tagName: String) = {
      val subs = str.substring(0, closeTagStartIndex)
      val index = subs.lastIndexOf(s"<$tagName>")
      if (index == -1) {
        subs.lastIndexOf(s"<$tagName ")
      } else {
        index
      }
    }
    /**
     * Modified Opening Tag - <aaa/>
     * @param current - index
     * @param acc - result
     * @return Set with tags and positions
     */
    def findModifiedOpeningTags(current: Int, acc: Seq[(String, Int, Int)]): Seq[(String, Int, Int)] = {
      val endIndex = str.indexOf("/>", current)
      if (endIndex == -1) {
        acc
      } else {
        val startIndex = str.substring(current, endIndex).lastIndexOf("<")
        if (startIndex == -1) {
          findModifiedOpeningTags(endIndex + 1, acc)
        } else {
          val tagName = searchForTagName(startIndex + 1 + current, endIndex)
          if (xml.Utility.isName(tagName)) {
            val xmlPart = (str.substring(startIndex + current, endIndex + 2), startIndex + current, endIndex + 2)
            findModifiedOpeningTags(endIndex + 2, xmlPart +: acc)
          } else {
            acc
          }
        }
      }
    }

    def searchForTagName(startIndex: Int, endIndex: Int) = {
      val subs = str.substring(startIndex, endIndex)
      val spaceIndex = subs.indexOf(' ', 1)
      if (spaceIndex == -1) {
        subs
      } else {
        subs.substring(0, spaceIndex)
      }
    }
    findModifiedOpeningTags(0, Seq.empty) ++ findXmlParts(0, Seq.empty)
  }
}