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
version := "2.0.2-DEV"

scalaVersion := "2.12.7"

licenses += "GPLv3" -> url("https://www.gnu.org/licenses/gpl-3.0.html")

homepage := Some(url("https://github.com/ziggystar/docman2"))

startYear := Some(2013)

description := "Application for managing PDF files with meta data"

//scala-swing
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.0.2"

//MigLayout
libraryDependencies += "com.miglayout" % "miglayout-swing" % "5.2"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.26.5"

//scala-arm
libraryDependencies += "com.jsuereth" %% "scala-arm" % "2.0"

//configration
libraryDependencies += "com.github.pureconfig" %% "pureconfig" % "0.9.2"

//icons
libraryDependencies += "com.github.jiconfont" % "jiconfont-typicons" % "2.0.7.0"

libraryDependencies += "com.github.jiconfont" % "jiconfont-swing" % "1.0.1"

//configuration serialization
libraryDependencies += "com.lihaoyi" %% "upickle" % "0.6.2"

//logging
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.8.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

//******** Java dependencies

//pdfbox
libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "2.0.8"


//generate properties file with version
resourceGenerators in Compile <+=
  (resourceManaged in Compile, name, version, streams) map { (dir, n, v, s) =>
    val file = dir / "docman" / "version.properties"
    s.log.info(s"generating version info in $file")
    val contents = "name=%s\nversion=%s".format(n,v)
    IO.write(file, contents)
    Seq(file)
  }
