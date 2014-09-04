package sbt

class CommentedXmlSpec extends CheckIfParsedSpec {

  override protected def files: Seq[(String, String, Boolean, Boolean)] = Seq(
    (
      s"""|
         |val pom = "</scm>"
         |
         |val aaa= <scm><url>git@github.com:mohiva/play.git</url>
         |    <cc>ewrer</cc>
         |  </scm>
         |
         |val tra = "</scm>"
         |
       """.stripMargin, "Xml in string", false, true),
    ("""
      |val scmpom = taskKey[xml.NodeBuffer]("Node buffer")
      |
      |scmpom := <scm>
      |    <url>git@github.com:mohiva/play-html-compressor.git</url>
      |    <connection>scm:git:git@github.com:mohiva/play-html-compressor.git</connection>
      |  </scm>
      | <developers>
      |    <developer>
      |      <id>akkie</id>
      |      <name>Christian Kaps</name>
      |      <url>http://mohiva.com</url>
      |    </developer>
      |  </developers>
      |  //<aaa/>
      |  <a></a>
      |
      |publishMavenStyle := true
      |
    """.stripMargin, "Wrong Commented xml ", false, true),
    ("""
      |val scmpom = taskKey[xml.NodeBuffer]("Node buffer")
      |
      |scmpom := <scm>
      |    <url>git@github.com:mohiva/play-html-compressor.git</url>
      |    <connection>scm:git:git@github.com:mohiva/play-html-compressor.git</connection>
      |  </scm>
      | <developers>
      |    <developer>
      |      <id>akkie</id>
      |      <name>Christian Kaps</name>
      |      <url>http://mohiva.com</url>
      |    </developer>
      |  </developers>
      |  //<aaa/>
      |  //<a></a>
      |
      |publishMavenStyle := true
      |
    """.stripMargin, "Commented xml ", false, true),
    ("""
      |import sbt._
      |
      |// </a
    """.stripMargin, "Xml in comment", true, false),
    ("""
      |// a/>
    """.stripMargin, "Xml in comment2", false, false)

  )
}
