package sbt

import java.io.File

import scala.util.{Failure, Success, Try}


case class SplitExpressionsNoBlankies(file: File, lines: Seq[String]) {
  val (imports, settings) = splitExpressions(file, lines)

  private def splitExpressions(file: File, lines: Seq[String]): (Seq[(String, Int)], Seq[(String, LineRange)]) = {
    import scala.reflect.runtime._
    import scala.reflect.runtime.universe._
    import scala.tools.reflect.ToolBox
    import scala.tools.reflect.ToolBoxError
    import scala.compat.Platform.EOL

    val mirror = universe.runtimeMirror(this.getClass.getClassLoader)
    val toolbox = mirror.mkToolBox(options = "-Yrangepos")
    val original = lines.mkString("\n")
    val merged = handleXmlContent(original)
//    if (merged != original) {
//      println(s"$merged")
//    }
    val parsed =
      try {
        toolbox.parse(merged)
      } catch {
        case e: ToolBoxError =>
          val seq = toolbox.frontEnd.infos.map { i =>
            val fileName = if (file == null) "Here should be file name" else file.getAbsolutePath
            s"""[${i.severity}]: [$fileName]:${i.pos.line}: ${i.msg}"""
          }
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
      (merged.substring(t.pos.start, t.pos.end), t.pos.line - 1)

    def convertStatement(t: Tree): Option[(String, LineRange)] = if (t.pos.isDefined) {
      val statement = merged.substring(t.pos.start, t.pos.end)
      val numberLines = statement.count(c => c == '\n')
//      println(
//        s"""$statement
//           |${t.children.mkString(EOL)}
//           |${(t.pos.start, t.pos.end)}
//           |${t.pos}
//           |""".stripMargin)
      Some((statement, LineRange(t.pos.line - 1, t.pos.line  + numberLines)))
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
    val statements: Seq[(String, Boolean)] = splitFile(str, to)
    val (builder, wasPreviousXml, wasXml, _) = statements.foldLeft((Seq.empty[String], false, false, "")) {
      (acc, el) =>
        val (bAcc, wasXml, _, previous) = acc
        val (content, isXml) = el
        val contentEmpty = content.trim.isEmpty
        val (isNotCommentedXml, newAcc) = if (isXml) {
          if (!wasXml) {
            if (addBracketsIfNecessary(previous)) {
              (true, " ( " +: bAcc)
            } else {
              (false, bAcc)
            }
          } else {
            (true, bAcc)
          }
        } else if (wasXml && !contentEmpty) {
          (false, " ) " +: bAcc)
        } else {
          (false, bAcc)
        }

        (content +: newAcc, isNotCommentedXml || (wasXml && contentEmpty), isXml, content)
    }
    val b = if (wasPreviousXml && !wasXml) {
      if (builder.headOption.exists(_.trim.isEmpty)) {
        builder.head +: " ) " +: builder.tail
      } else {
        " ) " +: builder
      }
    } else {
      builder
    }
    b.reverse.mkString
  }

  private def addBracketsIfNecessary(text: String): Boolean = {
    val doubleSlash = text.indexOf("//")
    val endOfLine = text.indexOf("\n")
    if (doubleSlash == -1 || (doubleSlash < endOfLine)) {
      val roundBrackets = text.lastIndexOf("(")
      val braces = text.lastIndexOf("{")
      val max = roundBrackets.max(braces)
      if (max == -1) {
        true
      } else {
        val trimmed = text.substring(max + 1).trim
        trimmed.nonEmpty
      }
    } else {
      false
    }
  }


  private def splitFile(content: String, ts: Seq[(String, Int, Int)]): Seq[(String, Boolean)] = {
    val (statements, index) = ts.foldLeft((Seq.empty[(String, Boolean)], 0)) {
      (accSeqIndex, el) =>
        val (statement, startIndex, endIndex) = el
        val (accSeq, index) = accSeqIndex
        val textStatementOption = if (index >= startIndex) {
          None
        } else {
          val s = content.substring(index, startIndex)
          Some((s, false))
        }
        val newAccSeq = (statement, true) +: addOptionToCollection(accSeq, textStatementOption)
        (newAccSeq, endIndex)
    }
    ((content.substring(index, content.length), false) +: statements).reverse
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
      val xmlFragment = findModifiedOpeningTag(content, offsetIndex, endIndex)
      val newAcc = addOptionToCollection(acc, xmlFragment)
      findModifiedOpeningTags(content, endIndex + 2, newAcc)
    }
  }

  private def findModifiedOpeningTag(content: String, offsetIndex: Int, endIndex: Int): Option[(String, Int, Int)] = {
    val startIndex = content.substring(offsetIndex, endIndex).lastIndexOf("<")
    if (startIndex == -1) {
      None
    } else {
      val tagName = searchForTagName(content, startIndex + 1 + offsetIndex, endIndex)
      if (xml.Utility.isName(tagName)) {
        xmlFragmentOption(content, startIndex + offsetIndex, endIndex + 2)
      } else {
        None
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
        val xmlFragment = findNotModifiedOpeningTag(content, closeTagStartIndex, closeTagEndIndex)
        val newAcc = addOptionToCollection(acc, xmlFragment)
        findNotModifiedOpeningTags(content, closeTagEndIndex + 1, newAcc)
      }
    }
  }

  private def addOptionToCollection[T](acc: Seq[T], option: Option[T]) = option.fold(acc)(el => el +: acc)

  private def findNotModifiedOpeningTag(content: String, closeTagStartIndex: Int, closeTagEndIndex: Int): Option[(String, Int, Int)] = {

    val tagName = content.substring(closeTagStartIndex + 2, closeTagEndIndex)
    if (xml.Utility.isName(tagName)) {
      val openTagIndex = searchForOpeningIndex(content, closeTagStartIndex, tagName)
      if (openTagIndex == -1) {
        None
      } else {
        xmlFragmentOption(content, openTagIndex, closeTagEndIndex + 1)
      }
    } else {
      None
    }

  }

  private def xmlFragmentOption(content: String, openIndex: Int, closeIndex: Int) = {
    val xmlPart = content.substring(openIndex, closeIndex)
    Try(xml.XML.loadString(xmlPart)) match {
      case Success(_) => Some((xmlPart, openIndex, closeIndex))
      case Failure(th) => None
    }
  }
}
