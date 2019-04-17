package docman.core

import java.time.{LocalDate, LocalDateTime}

case class Document[T](id: T,
                       sender: Option[String] = None,
                       subject: Option[String] = None,
                       tags: Set[String] = Set.empty,
                       date: Option[LocalDate] = None,
                       pages: Option[Int] = None,
                       lastModified: LocalDateTime = LocalDateTime.now(),
                       created: LocalDateTime = LocalDateTime.now() )

trait RODocumentStore[F[_]] {
  type Item
  type Id
  def getDocuments: F[Seq[Document[Id]]]
  def access(id: Id): F[Item]
}

trait DocumentStore[F[_]] extends RODocumentStore[F] {
  def updateDocument(d: Document[Id]): F[Document[Id]]
}
