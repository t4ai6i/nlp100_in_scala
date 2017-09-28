name := "nlp100_in_scala"

version := "1.0"

scalaVersion := "2.12.2"

scalacOptions ++= Seq(
  "-Xlint",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-unused",
  "-Ywarn-unused-import",
  "-Ywarn-value-discard",
  "-Xelide-below", "ALL"
)
scalacOptions in Test ++= Seq("-Yrangepos")
scalacOptions in (Compile, console) -= "-Xlint"
scalacOptions in (Compile, console) -= "-Ywarn-unused"
scalacOptions in (Compile, console) -= "-Ywarn-unused-import"

libraryDependencies ++= (Seq(
  "org.specs2" %% "specs2-core" % "3.9.5" % "test"
) ++ Seq( // scalaz
  "org.scalaz" %% "scalaz-core" % "7.2.8"
) ++ Seq( // scala-logging
  "com.typesafe.scala-logging" %% "scala-logging" % "3.7.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3"
) ++ Seq( // apache commons-io
  "commons-io" % "commons-io" % "2.5"
) ++ Seq( // typesafe playframework json
  "com.typesafe.play" %% "play-json" % "2.6.6"
))
