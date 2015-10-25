name := "docman2"

/*
1.0.1:
  - new Scala version 2.11.1 -> 2.11.5
 */
version := "1.0.1"


scalaVersion := "2.11.7"

//scala-swing
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"

libraryDependencies += "com.scalarx" %% "scalarx" % "0.2.4"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.25.0"

//scala-arm
libraryDependencies += "com.jsuereth" %% "scala-arm" % "1.4"

//******** Java dependencies

//pdfbox
libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "1.8.4"

//MigLayout
libraryDependencies += "com.miglayout" % "miglayout-swing" % "4.2"