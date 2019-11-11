package docman.backend.csv

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.time.LocalDateTime

import cats.effect.{Resource, Sync}
import cats.instances.all._
import cats.syntax.all._
import com.typesafe.scalalogging.StrictLogging
import docman.core.{Document, DocumentStore}

import scala.collection.JavaConverters._

case class CSVStore[F[_]: Sync](root: Path, dbFile: File) extends DocumentStore[F] with StrictLogging {
  override type Content = File
  /** Id is a full or relative path to a pdf file. It must be below one of the root directories. */
  type Id = Path

  var database: Map[Path, Document] = Map()

  logger.info(s"root: ${root.toRealPath()}")

  def normalizeFoundPath(p: Path): Path = root.toRealPath().relativize(p.toRealPath())

  override def updateDocument(id: Id, d: Doc): F[Doc] =
    for {
      _ <- Sync[F].delay {
        logger.debug(s"update data for $id to $d")
      }
      dUpdate = d.copy(lastModified = LocalDateTime.now())
      _ <- CSVHelpers.write(dbFile, id, dUpdate)
      _ <- Sync[F].delay {
        database = database + (id -> dUpdate)
      }
    } yield dUpdate

  override def reloadDB: F[Unit] =
    for {
      _ <- Sync[F].delay(logger.debug(s"load database from file $dbFile"))
      entries <- CSVHelpers.readFile(dbFile, createIfNotExists = false)
      _ <- Sync[F].delay(database = entries.toMap)
    } yield ()

  override def scanForPDFs: F[Unit] = for {
    _ <- Sync[F].delay(logger.debug(s"scan for new PDFs under $root"))
    newPDFs <- Sync[F].delay(
          Files.find(
            root,
            10,
            (p: Path, bfa: BasicFileAttributes) => bfa.isRegularFile && p.toString.toLowerCase.endsWith(".pdf"))
            .iterator().asScala.toIndexedSeq
            .map(root.relativize)
            .filterNot(database.contains)
        )
    newPDFsNormalized = newPDFs.map(normalizeFoundPath)
    _ <- newPDFsNormalized.map(f => updateDocument(f, Document())).toList.sequence
  } yield ()

  override def getAllDocuments: F[Seq[(Id, Doc)]] = Sync[F].pure(database.toSeq)

  override def access(id: Id): F[Content] = Sync[F].pure(root.resolve(id).toRealPath().toFile)
}

object CSVStore {
  def asResource[F[_]: Sync](root: Path, dbFile: File): Resource[F,CSVStore[F]] =
    Resource.liftF(Sync[F].delay{CSVStore(root, dbFile)})
}

