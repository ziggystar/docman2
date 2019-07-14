package docman.frontend.cli

import java.nio.file.{Files, Path, Paths}

import cats.implicits._
import com.monovore.decline._
import docman.backend.csv.CSVStore
import docman.backend.sidecar.SideCarRO

object CLI extends CommandApp(
  name = "docman-cli",
  header = "Document Manager",
  main = {
    Opts.subcommand(CLICommands.convertSCCommand)
  }
)

sealed trait DBFormat
case class SideCar(suffix: String, roots: Path) extends DBFormat
case class CSV(dbFile: Path) extends DBFormat

object CLICommands {

  def convertSCCommand: Command[Unit] = Command(
    name = "convsc",
    header = "convert from sidecar to csv",
    helpFlag = true
  ) {
    val scRoot = Opts.option[Path]("sc-root", "to scan for sidecar files", "r")
      .withDefault(Paths.get("."))
      .validate("sc-root is not a directory")(Files.isDirectory(_))
    val scSuffix = Opts.option[String]("sc-suffix", "file suffix for sidecar files", "s").withDefault(".smd")
    val csvRoot = Opts.option[Path]("csv-root", "root for relative paths from csv file", "c")
      .withDefault(Paths.get("."))
      .validate("csv-root is not a directory")(Files.isDirectory(_))
    val csvFile = Opts.option[Path]("csv-db", "csv db file")
      .validate("csv db file must not exist and will be created")(!Files.exists(_))
      .map(Paths.get(".").relativize(_))

    val scDb: Opts[SideCarRO] = scRoot.map(r => SideCarRO(Seq(r.toFile -> true)))
    val csvDb: Opts[CSVStore] = (csvRoot,csvFile.map(_.toFile)).mapN(CSVStore)

    (scDb,csvDb).mapN { (sd, csv) =>
      val task = for{
        _ <- sd.reloadDB
        docs <- sd.getAllDocuments
        _ <- docs.toList.map(d => csv.updateDocument(d._1.toPath, d._2)).sequence
      } yield ()

      task.value.unsafeRunSync().left.foreach(e => println("error: " + e))
    }
  }
}
