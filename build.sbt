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

scalaVersion := "2.12.8"

licenses += "GPLv3" -> url("https://www.gnu.org/licenses/gpl-3.0.html")

homepage := Some(url("https://github.com/ziggystar/docman2"))

startYear := Some(2013)

description := "Application for managing PDF files with meta data"

scalacOptions += "-Ypartial-unification"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")

resolvers += Resolver.sonatypeRepo("releases")

libraryDependencies += "io.reactivex" %% "rxscala" % "0.26.5"

libraryDependencies += "io.monix" %% "monix" % "3.0.0"

libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"

// https://mvnrepository.com/artifact/io.circe/circe-core
val circeVersion = "0.12.3"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

//scala-swing
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.2"

libraryDependencies += "com.monovore" %% "decline" % "1.0.0"

//configuration serialization
libraryDependencies += "com.lihaoyi" %% "upickle" % "0.6.2"

//logging
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"

//testing
// https://mvnrepository.com/artifact/org.specs2/specs2-scalacheck
libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "4.8.0" % Test

//********************* Java Dependencies ***************************//

//MigLayout
libraryDependencies += "com.miglayout" % "miglayout-swing" % "5.2"

//icons
libraryDependencies += "com.github.jiconfont" % "jiconfont-typicons" % "2.0.7.0"

libraryDependencies += "com.github.jiconfont" % "jiconfont-swing" % "1.0.1"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

//pdfbox
libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "2.0.17"

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin).
  settings(
      buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoPackage := "docman"
  )

mainClass in assembly := Some("docman.frontend.cli.CLI")