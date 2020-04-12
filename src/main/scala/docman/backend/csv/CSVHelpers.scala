package docman.backend.csv

import java.io.{File, FileOutputStream}
import java.nio.file.{Files, Path}

import cats.effect._
import cats.instances.all._
import cats.syntax.all._
import docman.core.Document
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

object CSVHelpers {
  /** Read in a CSV file. If there are multiple entries for a document, the last one counts.
    *
    * @param db A CSV file that contains one doc per line.
    * @return The list of read entries.
    */
  def readFile[F[_]: Sync](db: File, createIfNotExists: Boolean): F[List[(Path,Document)]] = for{
    _ <- touch[F](db)
    res <- Resource.fromAutoCloseable(Sync[F].delay(scala.io.Source.fromFile(db, "UTF8")))
      .use(bs =>
        Sync[F].delay(bs.getLines())
          .map(
            _.toList
              .map(l => parseLine(l).map(_.leftMap(f => new File(f).toPath)))
              .sequence
              .fold(m => throw new Throwable("parse error: " + m),identity)
          )
      )
  } yield res

  def touch[F[_]: Sync](f: File): F[Unit] =
    Sync[F].delay(Files.createFile(f.toPath)).unlessA(f.exists)

  /** Tries to relativize `file` against one path in `roots`.
    * @return Tuple of the matching root (first) and the relativized path of `file` against the found root.
    */
  def relativize(roots: Seq[Path], file: Path): Option[(Path,Path)] =
    roots.find(root => file.toAbsolutePath.startsWith(root.toAbsolutePath)).map(root => root -> root.relativize(file))

  /** Append one line to a db file.
    *
    * @param db DB file to write to.
    * @param docFile Relative path to the document file.
    * @param data Meta data to write.
    * @return Unit.
    */
  def write[F[_]: Sync](db: File, docFile: Path, data: Document):F[Unit] =
        Resource
          .fromAutoCloseable(Sync[F].delay(new FileOutputStream(db, true)))
          .use(fout => Sync[F].delay(fout.write((makeLine(docFile.toString, data) + "\n").getBytes("UTF8"))))

  def parseLine(line: String): Either[String,(String,Document)] = decode[(String,Document)](line).left.map(_.getMessage)
  def makeLine(f: String, d: Document): String = (f,d).asJson.noSpaces
}
