name := "nlp100_in_scala"

version := "1.0"

scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-Xlint",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  //  "-Ywarn-unused",
  //  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-Xelide-below", "ALL"
)
scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= (Seq(
  "org.specs2" %% "specs2-core" % "3.7" % "test"
) ++ Seq( // scala-logging
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3"
) ++ Seq( // apache commons-io
  "org.apache.commons" % "commons-io" % "1.3.2"
))

initialCommands in console := "import scalaz._, Scalaz._"

