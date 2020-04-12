package docman.frontend.cli

import java.nio.file.{Files, Path, Paths}

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import com.monovore.decline._
import docman.backend.csv.CSVStore
import docman.backend.sidecar.SideCarRO

object CLI extends CommandApp(
  name = "docman-cli",
  header = "Document Manager",
  main = {
    Opts.subcommand(CLICommands.convertSCCommand) orElse
    Opts.subcommand(CLICommands.guiCommand)
  }
)

sealed trait DBFormat
case class SideCar(suffix: String, roots: Path) extends DBFormat
case class CSV(dbFile: Path) extends DBFormat

object CLICommands {

  val dbFileOpt: Opts[Path] = Opts.option[Path]("dbfile", "data base file")
    .withDefault(Paths.get(System.getProperty("user.home", "")).resolve(".docman2/db.0.csv"))

  def guiCommand: Command[Unit] = Command(
    name = "gui",
    header = "run swing graphical user interface",
    helpFlag = true
  ) {
    Opts.unit
  }

  def convertSCCommand: Command[Unit] = Command(
    name = "convsc",
    header = "convert from sidecar to csv",
    helpFlag = true
  ) {
    val scRoot = Opts.option[Path]("sc-root", "to scan for sidecar files", "r")
      .withDefault(Paths.get("."))
      .validate("sc-root is not a directory")(Files.isDirectory(_))
    val csvRoot = Opts.option[Path]("csv-root", "root for relative paths from csv file", "c")
      .withDefault(Paths.get("."))
      .validate("csv-root is not a directory")(Files.isDirectory(_))
    val csvFile = dbFileOpt.validate("csv db file must not exist and will be created")(!Files.exists(_))

    val scDb: Opts[SideCarRO] = scRoot.map(r => SideCarRO(Seq(r.toFile -> true)))
    val csvDb: Opts[CSVStore[EitherT[IO,String,?]]] = (csvRoot,csvFile.map(_.toFile)).mapN(CSVStore(_,_))

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
