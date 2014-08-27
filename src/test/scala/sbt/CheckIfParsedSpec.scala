package sbt


abstract class CheckIfParsedSpec(implicit val splitter: SplitExpressions = new EvaluateConfigurationsScalania) extends AbstractSpec {

  this.getClass.getName should {

    "Parse sbt file " in {
      files.foreach { case (content,description, nonEmptyImports, nonEmptyStatements) =>
        println(s"""${getClass.getSimpleName}: "$description" """)
        val (imports, statements) = split(content)
        withClue(
          s"""$description
             |***${shouldContains(nonEmptyStatements)} statements***
             |$content """.stripMargin) {
          statements.nonEmpty shouldBe nonEmptyStatements
        }
        withClue(
          s"""$description
             |***${shouldContains(nonEmptyImports)} imports***
             |$content """.stripMargin) {
          imports.nonEmpty shouldBe nonEmptyImports
        }
      }
    }
  }

  private def shouldContains(b:Boolean) = s"""Should ${if(b){"contain"}else{"not contain"}}"""

  protected def files: Seq[(String,String, Boolean, Boolean)]

}
