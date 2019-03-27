package docman.engine

import java.io.File
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

import cats.data.EitherT
import cats.effect.IO
import cats.syntax.option._
import cats.syntax.either._
import docman.core.{DocAlgebra, Document, Sender, Tag}

import scala.collection.mutable
import scala.util.Try

case class SideCarRO(roots: Seq[(File,Boolean)]) extends DocAlgebra[EitherT[IO,String,?]]{
  type F[T] = EitherT[IO,String,T]

  def scanDir(d: File, recursive: Boolean): Seq[File] = {
    val (dirs,files) = Option(d.listFiles()).map(_.toSeq).getOrElse(Seq()).partition(_.isDirectory)
    files ++ (if(recursive) dirs.flatMap(scanDir(_, recursive = true)) else Seq())
  }

  val docs: mutable.Map[File, Document] = (for{
        (dir,rec) <- roots
        file <- scanDir(dir,rec) if file.getName.endsWith("pdf")
        sidecar = {
          val n = file.getAbsolutePath
          new File(n.take(n.lastIndexOf('.')) + ".smd").asRight.filterOrElse(_.exists(), s"sidecar for $n does not exist")
        }
        d = sidecar
          .flatMap(SideCarRO.readFile)
          .fold(e => {System.err.println(e); Document(0)}, identity)
  } yield file -> d)(collection.breakOut)

  def notSupported[T]: F[T] = EitherT.leftT("not supported")
  override def createSender(name: String): F[Sender] = notSupported
  override def createTag(name: String): F[Tag] = notSupported
  override def updateDocument(d: Document): F[Unit] = notSupported

  override def getDocuments: F[Seq[Document]] = EitherT.rightT(docs.values.toSeq)
}

object SideCarRO {
  case class Field[T](name: String, reader: String => T, update: T => Document => Document){
    def apply(s: String): Document => Document = update(reader(s))
  }

  val fields: Map[String,Field[_]] = Seq[Field[_]](
      Field[String]("SUBJECT", identity, s => _.copy(subject = s.some)),
      Field[LocalDate]("DATE", s => LocalDate.parse(s, DateTimeFormatter.ISO_LOCAL_DATE), d => _.copy(date = d.some)),
      Field[Set[Tag]]("TAGS", _.split(',').map(t => Tag(t.hashCode.toLong, t)).toSet, ts => _.copy(tags = ts)),
      Field[Sender]("AUTHOR", a => Sender(a.hashCode.toLong, a), a => _.copy(sender = a.some) )
    )
    .map(f => f.name -> f)
    .toMap

  def readFile(f: File): Either[String,Document] = {
    import resource._
    managed(io.Source.fromFile(f)).map(
      _.getLines()
        .map { line =>
          val (key,value) = line.splitAt(line.indexOf(':'))
          fields.get(key).map(_.apply(value.tail)).getOrElse(identity[Document](_))
        }
      .foldLeft(Document(f.hashCode()))((d,update) => update(d))
    ).either.left.map(_.map(_.getMessage).mkString(";"))
  }
}