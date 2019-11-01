package docman.backend.csv

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.time.LocalDateTime

import cats.data.EitherT
import cats.effect.IO
import cats.instances.all._
import cats.syntax.all._
import com.typesafe.scalalogging.StrictLogging
import docman.core.{Document, DocumentStore}

import scala.collection.JavaConverters._

case class CSVStore(root: Path, dbFile: File) extends DocumentStore[EitherT[IO,String,?]] with StrictLogging {
  override type Content = File
  /** Id is a full or relative path to a pdf file. It must be below one of the root directories. */
  type Id = Path

  var database: Map[Path, Document] = Map()

  logger.info(s"root: ${root.toRealPath()}")

  def normalizeFoundPath(p: Path): Path = root.toRealPath().relativize(p.toRealPath())

  override def updateDocument(id: Id, d: Doc): EitherT[IO, String, Doc] =
    for {
      _ <- EitherT.right(IO(logger.debug(s"update data for $id to $d")))
      dUpdate = d.copy(lastModified = LocalDateTime.now())
      _ <- CSVHelpers.write(dbFile, id, dUpdate)
      _ <- EitherT.right(IO{database = database + (id -> dUpdate)})
    } yield dUpdate

  override def reloadDB: EitherT[IO, String, Unit] =
    for {
      _ <- EitherT.right(IO(logger.debug(s"load database from file $dbFile")))
      entries <- CSVHelpers.readFile(dbFile, createIfNotExists = false)
      _ <- EitherT.right(IO{database = entries.toMap})
    } yield ()

  override def scanForPDFs: EitherT[IO, String, Unit] = for {
    _ <- EitherT.right(IO(logger.debug(s"scan for new PDFs under $root")))
    newPDFs <- EitherT(IO(
          Files.find(
            root,
            10,
            (p: Path, bfa: BasicFileAttributes) => bfa.isRegularFile && p.toString.toLowerCase.endsWith(".pdf"))
            .iterator().asScala.toIndexedSeq
            .map(root.relativize)
            .filterNot(database.contains)
        ).attempt.map(_.leftMap(_.getMessage)))
    newPDFsNormalized = newPDFs.map(normalizeFoundPath)
    _ <- newPDFsNormalized.map(f => updateDocument(f, Document())).toList.sequence
  } yield ()

  override def getAllDocuments: EitherT[IO, String, Seq[(Id, Doc)]] = EitherT.pure(database.toSeq)

  override def access(id: Id): EitherT[IO, String, Content] = EitherT.pure(root.resolve(id).toRealPath().toFile)
}

