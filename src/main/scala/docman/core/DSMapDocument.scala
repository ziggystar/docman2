package docman.core

import cats.Functor

case class DSMapDocument[F[_]: Functor, C, I, T](ds: DocumentStore[F]{type Id = I; type Content = C; type Doc = T},
                                              onLoad: T => T,
                                              onStore: T => T) extends DocumentStore[F] {
  import cats.syntax.all._
  override type Content = C
  override type Id = I
  type Doc = T

  override def updateDocument(id: Id, d: Doc): F[Doc] = ds.updateDocument(id, onStore(d))
  override def getAllDocuments: F[Seq[(Id, Doc)]] = ds.getAllDocuments.map(_.map(id => (id._1, onLoad(id._2))))

  override def reloadDB: F[Unit] = ds.reloadDB

  override def scanForPDFs: F[Seq[I]] = ds.scanForPDFs

  override def access(id: Id): F[Content] = ds.access(id)
}
