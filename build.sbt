name := "docman2"

/*
1.0.1:
  - new Scala version 2.11.1 -> 2.11.5
1.1.0:
 - migrate to rxscala
 */
version := "1.1.0-DEV"

scalaVersion := "2.11.8"

licenses += "GPLv3" -> url("https://www.gnu.org/licenses/gpl-3.0.html")

homepage := Some(url("https://github.com/ziggystar/docman2"))

startYear := Some(2013)

description := "Application for managing PDF files with meta data"

//scala-swing
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.2"

libraryDependencies += "com.scalarx" %% "scalarx" % "0.2.4"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.26.0"

//scala-arm
libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

//icons
libraryDependencies += "com.github.jiconfont" % "jiconfont-typicons" % "2.0.7.0"

libraryDependencies += "com.github.jiconfont" % "jiconfont-swing" % "1.0.1"

//logging
libraryDependencies += "com.typesafe.scala-logging" % "scala-logging-slf4j_2.11" % "2.1.2"

libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.18"


//to avoid conflict, I pick a version for scala-reflect
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8"

//******** Java dependencies

//pdfbox
libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "1.8.11"

//MigLayout
libraryDependencies += "com.miglayout" % "miglayout-swing" % "5.0"

//generate properties file with version
resourceGenerators in Compile <+=
  (resourceManaged in Compile, name, version, streams) map { (dir, n, v, s) =>
    val file = dir / "docman" / "version.properties"
    s.log.info(s"generating version info in $file")
    val contents = "name=%s\nversion=%s".format(n,v)
    IO.write(file, contents)
    Seq(file)
  }
