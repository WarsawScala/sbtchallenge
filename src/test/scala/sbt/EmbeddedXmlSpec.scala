package sbt


class EmbeddedXmlSpec extends CheckIfParsedSpec {

  "File with xml content " should {


    "Handle last xml part" in {
      val errorLine = """<version>4.0<version>"""
      val buildSbt = s"""|
                         |
                         |name := "play-html-compressor"
                         |
                         |scalaVersion := "2.11.1"
                         |
                         |val pom = <scm>
                         |<url>git@github.com:mhiva/play-html-compressor.git</url>
                         |<connection>scm:git:git@github.com:mohiva/play-html-compressor.git</connection>
                         |  </scm>
                         |<developers>
                         |    <developer>
                         |      <id>akkie</id>
                         |      <name>Christian Kaps</name>
                         |      <url>http://mohiva.com</url>
                         |    </developer>
                         |  </developers>
                         |$errorLine
                         |
                         |""".stripMargin

      split(buildSbt) must throwA[MessageOnlyException].like {
        case exception =>
          val index = buildSbt.lines.indexWhere(line => line.contains(errorLine)) + 1
          val numberRegex = """(\d+)""".r
          val message = exception.getMessage
          val list = numberRegex.findAllIn(message).toList
          list must contain(index.toString)
      }


    }


  }

  protected val files = Seq(
    ( """
        |val p = <a/>
      """.stripMargin, "Xml modified closing tag at end of file", false, true),
    ( """
        |val p = <a></a>
      """.stripMargin, "Xml at end of file", false, true),
    ( """|
        |
        |name := "play-html-compressor"
        |
        |scalaVersion := "2.11.1"
        |
        |val lll = "</=+=>"
        |
        |val not = "<sss><sss>"
        |
        |val aaa = "ass/>"
        |
        |val pom = "</scm>"
        |
        |val aaa= <scm><url>git@github.com:mohiva/play-html-compressor.git</url>
        |   <connection>scm:git:git@github.com:mohiva/play-html-compressor.git</connection>
        |  </scm>
        |  <developers>
        |    <developer>
        |      <id>akkie</id>
        |      <name>Christian Kaps</name>
        |      <url>http://mohiva.com</url>
        |    </developer>
        |  </developers>
        |  <version>4.0</version>
        |
        |publishMavenStyle := true
        |
        |val anotherXml = <a a="r"><bbb>
        |        content</bbb>
        |        <ccc atr="tre" />
        |        <aa/>
        |          </a>
        |
        |val tra = "</scm>"
        |
      """.stripMargin, "Xml in string", false, true),
    ( """|
        |
        |name := "play-html-compressor"
        |
        |scalaVersion := "2.11.1"
        |
        |val ok = <ccc atr="tre" />
        |
        |val pom = <scm>
        |<url>git@github.com:mhiva/play-html-compressor.git</url>
        |    <connection>scm:git:git@github.com:mohiva/play-html-compressor.git</connection>
        |</scm>
        |<developers>
        |<developer>
        |<id>akkie</id>
        |<name>Christian Kaps</name>
        |<url>http://mohiva.com</url>
        |</developer>
        |</developers>
        |<version>4.0</version>
        |
        |publishMavenStyle := true
        |
        |val anotherXml = <a a="r"><bbb>
        |        content</bbb>
        |        <ccc atr="tre" />
        |<aa/>
        | </a>
        |
        | """.stripMargin, "Xml with attributes", false, true)
  )

}
