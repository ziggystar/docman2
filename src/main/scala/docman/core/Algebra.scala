package docman.core

import java.time.{LocalDate, LocalDateTime}

case class Sender(id: Long, name: String)
case class Tag(id: Long, name: String)
case class Document(id: Long,
                    sender: Option[Sender] = None,
                    subject: Option[String] = None,
                    tags: Set[Tag] = Set(),
                    date: Option[LocalDate] = None,
                    pages: Int = 0,
                    lastModified: LocalDateTime = LocalDateTime.now(),
                    created: LocalDateTime = LocalDateTime.now() )

trait DocAlgebra[F[_]] {
  def createSender(name: String): F[Sender]
  def createTag(name: String): F[Tag]
  def getDocuments: F[Seq[Document]]
  def updateDocument(d: Document): F[Unit]
}
