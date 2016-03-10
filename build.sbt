name := "docman2"

/*
1.0.1:
  - new Scala version 2.11.1 -> 2.11.5
1.1.0:
 - migrate to rxscala
 */
version := "1.1.0-DEV"

scalaVersion := "2.11.8"

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
(resourceGenerators in Compile) += Def.task {
  val file = resourceManaged.value / "docman" / "version.properties"
  IO.write(file, """version=""" + version.value)
  Seq(file)
}.taskValue