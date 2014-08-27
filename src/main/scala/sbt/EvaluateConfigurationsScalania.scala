package sbt

class EvaluateConfigurationsScalania extends SplitExpressions {
  def splitExpressions(lines: Seq[String]): (Seq[(String, Int)], Seq[(String, LineRange)]) = {
    val split = SplitExpressionsNoBlankies(null, lines)
    (split.imports, split.settings)
  }
}