package docman.backend.sidecar

import java.io.File
import java.nio.file.Files
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import java.util.TimeZone

import cats.data.EitherT
import cats.effect.{IO, Resource}
import cats.syntax.either._
import cats.syntax.option._
import docman.core.{Document, RODocumentStore}

import scala.util.Try

case class SideCarRO(roots: Seq[(File,Boolean)]) extends RODocumentStore[EitherT[IO,String,?]]{
  type F[T] = EitherT[IO,String,T]
  override type Content = File
  override type Id = File


  def loadSideCarFiles: IO[Map[File,Document]] = IO{
    (for{
      (dir,rec) <- roots
      file <- SideCarRO.scanDir(dir,rec) if file.getName.endsWith("pdf")
      sidecar = {
        val n = file.getAbsolutePath
        new File(n.take(n.lastIndexOf('.')) + ".smd").asRight.filterOrElse(_.exists(), s"sidecar for $n does not exist")
      }
      d = sidecar
        .flatMap(SideCarRO.readFile)
        .fold(e => {System.err.println(e); Document()}, identity)
    } yield file -> d)(collection.breakOut)
  }

  var docs: Map[File, Document] = loadSideCarFiles.unsafeRunSync()

  def notSupported[T]: F[T] = EitherT.leftT("not supported")
  override def getAllDocuments: F[Seq[(File,Document)]] = EitherT.rightT(docs.toSeq)
  override def access(id: File): EitherT[IO, String, File] = EitherT.right(IO(id))
  override def reloadDB: EitherT[IO, String, Unit] = EitherT.liftF(loadSideCarFiles.map(m => {docs = m}))
  override def scanForPDFs: EitherT[IO, String, Unit] = notSupported
}

object SideCarHelpers {
  def dateReader: String => Either[String,LocalDate] =
    s => Try(LocalDate.parse(s, DateTimeFormatter.ISO_LOCAL_DATE)).toEither.leftMap(_.getMessage)
  def fileModificationDate: File => LocalDateTime = f =>
    LocalDateTime.ofInstant(Files.getLastModifiedTime(f.toPath).toInstant,TimeZone.getDefault.toZoneId)
}
object SideCarRO {
  case class Field[T](name: String, reader: String => T, update: T => Document => Document){
    def apply(s: String): Document => Document = update(reader(s))
  }
  val fields: Map[String,Field[_]] = Seq[Field[_]](
      Field[String]("SUBJECT", identity, s => _.copy(subject = s.some)),
      Field[Either[String,LocalDate]]("DATE", SideCarHelpers.dateReader, ed => _.copy(date = ed.right.toOption)),
      Field[Set[String]]("TAGS", _.split(',').toSet, ts => _.copy(tags = ts)),
      Field[String]("AUTHOR", identity[String], a => _.copy(sender = a.some) )
    )
    .map(f => f.name -> f)
    .toMap

  val modified: File => Document => Document = (f: File) => (d: Document) =>
    d.copy(lastModified = LocalDateTime.ofInstant(Files.getLastModifiedTime(f.toPath).toInstant,TimeZone.getDefault.toZoneId))

  def readFile(f: File): Either[String,Document] =
    Resource.fromAutoCloseable(IO(scala.io.Source.fromFile(f))).use(bs =>
      IO(
        bs.getLines()
          .map { line =>
            val (key,value) = line.splitAt(line.indexOf(':'))
            fields.get(key).map(_.apply(value.tail)).getOrElse(identity[Document](_))
          }
          .foldLeft(
            modified(f)(Document())
          )((d,update) => update(d))
      )
    )
      .attempt.map(_.leftMap(_.getMessage))
      .unsafeRunSync()

  def scanDir(d: File, recursive: Boolean): Seq[File] = {
    val (dirs,files) = Option(d.listFiles()).map(_.toSeq).getOrElse(Seq()).partition(_.isDirectory)
    files ++ (if(recursive) dirs.flatMap(scanDir(_, recursive = true)) else Seq())
  }
}