package docman.frontend.swing

import java.nio.file.{Files, Path, Paths}

import cats.implicits._
import com.monovore.decline._
import docman.BuildInfo
import javax.swing.{Action => _, _}

object Main extends CommandApp(
  name="docman2",
  header = "PDF management application",
  main = {
    val db = Opts.option[Path]("dbfile", "data base file")
      .withDefault(Paths.get(System.getProperty("user.home", "")).resolve(".docman2/db.0.csv"))
      .map(_.toFile)
    val root = Opts.argument[Path]("root").validate("document root must be directory")(Files.isDirectory(_))
    (root,db).mapN(Config).map(AppMain(_))
  },
  version = BuildInfo.version
)
