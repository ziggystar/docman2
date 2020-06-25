name := "docman2"

/*
1.0.1:
  - new Scala version 2.11.1 -> 2.11.5
1.1.0:
 - migrate to rxscala
2.0.0:
 - search multiple directories
 - display tags; click them for filtering
2.0.1:
 - fix: most frequent tags are NOT displayed
 */
version := "3.0.0-DEV"

scalaVersion := "2.13.2"

licenses += "GPLv3" -> url("https://www.gnu.org/licenses/gpl-3.0.html")

homepage := Some(url("https://github.com/ziggystar/docman2"))

startYear := Some(2013)

description := "Application for managing PDF files with meta data"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "io.monix" %% "monix" % "3.2.2"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.1.3"

// https://mvnrepository.com/artifact/io.circe/circe-core
val circeVersion = "0.13.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies += "com.monovore" %% "decline-effect" % "1.2.0"

//logging
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

//java stuff below

//pdfbox
libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "2.0.20"

//MigLayout
libraryDependencies += "com.miglayout" % "miglayout-swing" % "5.2"

//icons
libraryDependencies += "com.github.jiconfont" % "jiconfont-font_awesome" % "4.7.0.1"

libraryDependencies += "com.github.jiconfont" % "jiconfont-swing" % "1.0.1"

//testing
// https://mvnrepository.com/artifact/org.specs2/specs2-scalacheck
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "4.10.0" % Test

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "docman"
  )

mainClass in assembly := Some("docman.frontend.cli.CLI")

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-feature",
  "-deprecation",
  "-unchecked",
  "-Xlint",
  "-Xfatal-warnings",
  "-language:implicitConversions",
  "-language:higherKinds",
  "-language:existentials",
  "-language:postfixOps"
)
