package docman.engine.csv

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path}
import java.time.LocalDateTime

import cats.data.EitherT
import cats.effect.IO
import docman.core.{Document, DocumentStore}

import scala.collection.JavaConverters._

case class CSVStore(root: Path, dbFile: File) extends DocumentStore[EitherT[IO,String,?]] {
  override type Content = File
  /** Id is a full or relative path to a pdf file. It must be below one of the root directories. */
  type Id = Path

  var database: Map[Path, Document] = Map()

  override def updateDocument(id: Id, d: Doc): EitherT[IO, String, Doc] = for {
    dUpdate <- EitherT.liftF(IO(d.copy(lastModified = LocalDateTime.now())))
    _ <- CSVHelpers.write(dbFile, id, dUpdate)
    _ <- EitherT.right(IO{database = database + (id -> dUpdate)})
  } yield dUpdate

  override def reloadDB: EitherT[IO, String, Unit] =
    for{
      entries <- CSVHelpers.readFile(dbFile, createIfNotExists = false)
      _ <- EitherT.right(IO{database = entries.toMap})
    } yield ()

  override def scanForPDFs: EitherT[IO, String, Seq[Id]] = for{
    allPDFs <- EitherT.right(IO(
        Files.find(root, 10, (p: Path, bfa: BasicFileAttributes) => bfa.isRegularFile && p.toString.toLowerCase.endsWith(".pdf")).iterator().asScala.toSeq
    ))
  } yield allPDFs
  override def getAllDocuments: EitherT[IO, String, Seq[(Id, Doc)]] = EitherT.pure(database.toSeq)
  override def access(id: Id): EitherT[IO, String, Content] = EitherT.pure(root.resolve(id).toFile)
}

