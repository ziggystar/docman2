package docman.core

import java.time.{LocalDate, LocalDateTime}

import docman.backend.csv.Storable

/** New version of document object. */
case class Document(sender: Option[String] = None,
                    subject: Option[String] = None,
                    tags: Set[String] = Set.empty,
                    date: Option[LocalDate] = None,
                    pages: Option[Int] = None,
                    lastModified: LocalDateTime = LocalDateTime.now(),
                    created: LocalDateTime = LocalDateTime.now() )
object Document {
  import io.circe.generic.semiauto._
  import io.circe.{Codec, Decoder, Encoder}

  implicit def decoder: Decoder[Document] = deriveDecoder[Document]
  implicit def encoder: Encoder[Document] = deriveEncoder[Document]
  implicit def codec: Codec[Document] = Codec.from(decoder, encoder)

  implicit def storable: Storable[Document] = Storable[Document](implicitly[Codec[Document]], (t: LocalDateTime) => Document(created = t), (t: LocalDateTime) => (_: Document).copy(lastModified = t))
}



trait RODocumentStore[F[_]] {
  type Content
  type Id
  type Doc
  def reloadDB: F[Unit]
  def scanForPDFs: F[Seq[Id]]
  def getAllDocuments: F[Seq[(Id,Doc)]]
  def access(id: Id): F[Content]
}

trait DocumentStore[F[_]] extends RODocumentStore[F] {
  def updateDocument(id: Id, d: Doc): F[Doc]
}

