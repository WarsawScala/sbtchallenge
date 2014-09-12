package sbt


object SessionSettings {

  def newLines(oldContentWithIndex: List[(String, Int)], lineMap: Map[Int, (Int, List[String])]): List[String] = {
    lineMap.foreach {
      case (from, (to, lines)) => println(
        s"""[$from - $to]
           | $lines""".stripMargin)
    }
    println("-------------")
    oldContentWithIndex.foreach { case (el, index) => println(s"$index: $el")}
    println("-------------")
    toLines(oldContentWithIndex,lineMap)
  }


  private[sbt] def toLines(oldContentWithIndex: List[(String, Int)], lineMap: Map[Int, (Int, List[String])]): List[String] = {
    val (tmpLines, _) = ((List[String](), 1) /: oldContentWithIndex) {
      case ((accLines, n), (line, m)) if n == m + 1 =>
        lineMap.get(n) match {
          case Some(Pair(end, lines)) => (lines reverse_::: accLines, end)
          case None => (line :: accLines, n + 1)
        }
      case (res, _) => res
    }
    tmpLines.reverse
  }
}
