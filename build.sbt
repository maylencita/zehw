name := "Giterly"

version := "1.0"

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)

TwirlKeys.templateImports ++= Seq("forms._", "org.joda.time.DateTime", "org.joda.time.format._")

libraryDependencies ++= Seq(
  ws,
  "joda-time" % "joda-time" % "2.5"
)
