package docman.core

import java.time.{LocalDate, LocalDateTime}

import cats._
import cats.syntax.functor._
import cats.instances.tuple._
import cats.syntax.bifunctor._
import shapeless.ops.hlist.Mapped
import shapeless.{Const, HList}

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
  def getDocuments: F[Seq[(Id,Doc)]]
  def access(id: Id): F[Content]
}

trait DocumentStore[F[_]] extends RODocumentStore[F] {
  def updateDocument(d: Document): F[Document]
}

trait ColumnModelRO[F[_]] extends RODocumentStore[F]{
  /** Columns including the id column. */
  type HDoc <: HList
  type Id
  type Content
  type Doc
  type NameList = Mapped[HDoc,Const[String]#λ]#Out
  def names: NameList
  def toDocument(ro: HDoc): Doc
  def toColumns(d: Doc): HDoc
  def getColumns: F[Seq[(Id,HDoc)]]
  def getDocuments(implicit f: Functor[F]): F[Seq[(Id,Doc)]] = getColumns.map(_.map(_.bimap(identity,toDocument)))
  def access(id: Id): F[Content]
}
