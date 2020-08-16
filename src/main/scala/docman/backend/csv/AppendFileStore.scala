package docman.backend.csv

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.time.{LocalDateTime, ZoneId}

import cats.effect.{Resource, Sync}
import cats.instances.all._
import cats.syntax.all._
import docman.core.DocumentStore
import docman.utils.Logging
import io.circe.Codec

import scala.jdk.CollectionConverters._

case class Storable[T](codec: Codec[T], create: LocalDateTime => T, modify: LocalDateTime => T => T)
object Storable {
  def apply[T : Storable] = implicitly[Storable[T]]
}

case class AppendFileStore[F[_]: Sync,T : Storable](root: Path, dbFile: File, createDbIfNotExists: Boolean) extends DocumentStore[F] with Logging {
  override type Content = File
  /** Id is a full or relative path to a pdf file. It must be below one of the root directories. */
  type Id = Path
  type Doc = T

  implicit def codec: Codec[T] = Storable[T].codec

  var database: Map[Path, T] = Map()

  logger.info(s"root: ${root.toRealPath()}")

  def normalizeFoundPath(p: Path): Path = root.toRealPath().relativize(p.toRealPath())

  override def updateDocument(id: Id, d: T): F[T] =
    for {
      _ <- Sync[F].delay {
        logger.debug(s"update data for $id to $d")
      }
      dUpdate = Storable[T].modify(LocalDateTime.now())(d)
      _ <- CSVHelpers.write(dbFile, id, dUpdate)
      _ <- Sync[F].delay {
        database = database + (id -> dUpdate)
      }
    } yield dUpdate

  override def reloadDB: F[Unit] =
    for {
      _ <- Sync[F].delay(logger.info(s"load database from file $dbFile"))
      entries <- CSVHelpers.readFile(dbFile, createIfNotExists = createDbIfNotExists).recover(_ => Nil)
      _ <- Sync[F].delay {
        database = entries.toMap
      }
    } yield ()

  override def scanForPDFs: F[Seq[Id]] = for {
    _ <- Sync[F].delay(logger.debug(s"scan for new PDFs under $root"))
    newPDFs <- Sync[F].delay(
          Files.find(
            root,
            10,
            (p: Path, bfa: BasicFileAttributes) => bfa.isRegularFile && p.toString.toLowerCase.endsWith(".pdf"))
            .iterator().asScala.toIndexedSeq
        )
    withDate = newPDFs.map(f => (f,LocalDateTime.ofInstant(Files.getLastModifiedTime(f).toInstant, ZoneId.systemDefault())))
    newPDFsNormalized = withDate.map(_.leftMap(normalizeFoundPath)).filterNot(x => database.contains(x._1))
    _ <- newPDFsNormalized.map(f => updateDocument(f._1, Storable[T].create(f._2))).toList.sequence
  } yield newPDFsNormalized.map(_._1)

  override def getAllDocuments: F[Seq[(Id, T)]] = Sync[F].delay(database.toSeq)

  override def access(id: Id): F[Content] = Sync[F].pure(root.resolve(id).toRealPath().toFile)
}

object AppendFileStore {
  def asResource[F[_]: Sync, T : Storable](root: Path, dbFile: File, createDbIfNotExists: Boolean = false): Resource[F,AppendFileStore[F,T]] =
    Resource.liftF(Sync[F].delay{AppendFileStore(root, dbFile, createDbIfNotExists)})
}

