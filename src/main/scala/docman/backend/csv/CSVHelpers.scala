package docman.backend.csv

import java.io.{File, FileNotFoundException, FileOutputStream}
import java.nio.file.{Files, Path}

import cats.effect._
import cats.instances.all._
import cats.syntax.all._
import docman.utils.Logging
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Codec, Decoder, Encoder}

object CSVHelpers extends Logging {
  /** Read in a CSV file. If there are multiple entries for a document, the last one counts.
    *
    * @param db A CSV file that contains one doc per line.
    * @return The list of read entries.
    */
  def readFile[F[_]: Sync,T : Codec](db: File, createIfNotExists: Boolean): F[List[(Path,T)]] = for{
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
  def write[F[_]: Sync, T: Encoder](db: File, docFile: Path, data: T):F[Unit] =
        Resource
          .fromAutoCloseable(
            Sync[F].delay(new FileOutputStream(db, true))
              .recoverWith{case e: FileNotFoundException => Sync[F].delay{
                if(db.getParentFile.mkdirs())
                  {
                    logger.info(s"creating directory ${db.getParentFile}")
                    new FileOutputStream(db, true)
                  }
                else
                  throw e
              }})
          .use(fout => Sync[F].delay(fout.write((makeLine(docFile.toString, data) + "\n").getBytes("UTF8"))))

  def parseLine[T: Decoder](line: String): Either[String,(String,T)] = decode[(String,T)](line).left.map(_.getMessage)
  def makeLine[T: Encoder](f: String, d: T): String = (f,d).asJson.noSpaces
}
