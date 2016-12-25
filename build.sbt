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

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.7" % "test"
)
initialCommands in console := "import scalaz._, Scalaz._"

scalacOptions in Test ++= Seq("-Yrangepos")
