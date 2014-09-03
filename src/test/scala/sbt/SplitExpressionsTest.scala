package sbt

import org.specs2.mutable.Specification

class SplitExpressionsTest extends Specification with SplitExpressionsBehavior {

  "EvaluateConfigurationsOriginal" should  oldExpressionsSplitter(new EvaluateConfigurationsOriginal)
  
  "EvaluateConfigurationsScalania" should  oldExpressionsSplitter(new EvaluateConfigurationsScalania)
  
  "EvaluateConfigurationsScalania" should newExpressionsSplitter(new EvaluateConfigurationsScalania)
  
}