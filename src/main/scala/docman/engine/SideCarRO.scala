package docman.engine

import java.io.File
import java.nio.file.Files
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import java.util.TimeZone

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.either._
import cats.syntax.option._
import docman.core.{RODocumentStore, Document}

import scala.collection.mutable

case class SideCarRO(roots: Seq[(File,Boolean)]) extends RODocumentStore[EitherT[IO,String,?]]{
  type F[T] = EitherT[IO,String,T]
  override type Item = File
  override type Id = File

  def scanDir(d: File, recursive: Boolean): Seq[File] = {
    val (dirs,files) = Option(d.listFiles()).map(_.toSeq).getOrElse(Seq()).partition(_.isDirectory)
    files ++ (if(recursive) dirs.flatMap(scanDir(_, recursive = true)) else Seq())
  }

  val docs: mutable.Map[File, Document[File]] = (for{
        (dir,rec) <- roots
        file <- scanDir(dir,rec) if file.getName.endsWith("pdf")
        sidecar = {
          val n = file.getAbsolutePath
          new File(n.take(n.lastIndexOf('.')) + ".smd").asRight.filterOrElse(_.exists(), s"sidecar for $n does not exist")
        }
        d = sidecar
          .flatMap(SideCarRO.readFile)
          .fold(e => {System.err.println(e); Document[File](new File(""))}, identity)
  } yield file -> d)(collection.breakOut)

  def notSupported[T]: F[T] = EitherT.leftT("not supported")
  override def getDocuments: F[Seq[Document[File]]] = EitherT.rightT(docs.values.toSeq)
  override def access(id: File): EitherT[IO, String, File] = EitherT.right(IO(id))
}

object SideCarRO {
  case class Field[T](name: String, reader: String => T, update: T => Document[File] => Document[File]){
    def apply(s: String): Document[File] => Document[File] = update(reader(s))
  }
  val fields: Map[String,Field[_]] = Seq[Field[_]](
      Field[String]("SUBJECT", identity, s => _.copy(subject = s.some)),
      Field[LocalDate]("DATE", s => LocalDate.parse(s, DateTimeFormatter.ISO_LOCAL_DATE), d => _.copy(date = d.some)),
      Field[Set[String]]("TAGS", _.split(',').toSet, ts => _.copy(tags = ts)),
      Field[String]("AUTHOR", identity[String], a => _.copy(sender = a.some) )
    )
    .map(f => f.name -> f)
    .toMap

  val modified: File => Document[File] => Document[File] = (f: File) => (d: Document[File]) =>
    d.copy(lastModified = LocalDateTime.ofInstant(Files.getLastModifiedTime(f.toPath).toInstant,TimeZone.getDefault.toZoneId))

  def readFile(f: File): Either[String,Document[File]] = {
    import resource._
    managed(io.Source.fromFile(f)).map(
      _.getLines()
        .map { line =>
          val (key,value) = line.splitAt(line.indexOf(':'))
          fields.get(key).map(_.apply(value.tail)).getOrElse(identity[Document[File]](_))
        }
      .foldLeft(
        modified(f)(Document(id = f))
      )((d,update) => update(d))
    ).either.left.map(_.map(_.getMessage).mkString(";"))
  }
}