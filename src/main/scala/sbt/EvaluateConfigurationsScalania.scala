package sbt

import java.io.File

object EvaluateConfigurations {
  def splitExpressions(file: File, lines: Seq[String]): (Seq[(String, Int)], Seq[(String, LineRange)]) = {
    val split = SplitExpressionsNoBlankies(file, lines)
    (split.imports, split.settings)
  }
}