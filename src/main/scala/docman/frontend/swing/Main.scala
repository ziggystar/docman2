package docman.frontend.swing

import java.io.File
import java.nio.file.Paths

import cats.implicits._
import com.monovore.decline._
import docman.BuildInfo
import javax.swing.{Action => _, _}

object Main extends CommandApp(
  name="docman2",
  header = "PDF management application",
  main = {
    val db = Opts.option[String]("dbfile", "data base file")
      .withDefault(System.getProperty("user.home", "") + "/.docman2/db.0.csv")
      .map(new File(_))
    val root = Opts.argument[String]("root").map(Paths.get(_))
    (root,db).mapN(Config(_,_)).map(AppMain(_))
  },
  version = BuildInfo.version
)
