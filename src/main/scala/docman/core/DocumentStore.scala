package docman.core

import java.time.{LocalDate, LocalDateTime}

import cats.Functor

/** New version of document object. */
case class Document(sender: Option[String] = None,
                    subject: Option[String] = None,
                    tags: Set[String] = Set.empty,
                    date: Option[LocalDate] = None,
                    pages: Option[Int] = None,
                    lastModified: LocalDateTime = LocalDateTime.now(),
                    created: LocalDateTime = LocalDateTime.now() )



trait RODocumentStore[F[_]] {
  type Content
  type Id
  type Doc = Document
  def reloadDB: F[Unit]
  def scanForPDFs: F[Unit]
  def getAllDocuments: F[Seq[(Id,Doc)]]
  def access(id: Id): F[Content]
}

trait DocumentStore[F[_]] extends RODocumentStore[F] {
  def updateDocument(id: Id, d: Document): F[Document]
}

