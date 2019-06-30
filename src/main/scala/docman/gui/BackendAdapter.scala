package docman.gui

import java.io.File
import java.sql.Date

import cats.data.EitherT
import cats.effect.IO
import docman.core._
import docman.utils.Isomorphism
import rx.lang.scala.{Observable, Subject}

/** Bridge from old code to functional backend. */

case class BackendAdapter[IdT](backend: DocumentStore[EitherT[IO,String,?]]{type Id = IdT}, file: Isomorphism[IdT,File]){
  type DocT = Doc
  private def documentToDoc(id: IdT, document: Document): DocT = {
    val pm = DProp.ALL.foldLeft(PropertyMap.empty){ case (map, dp) =>
      if(dp.name == DateDP.name) {
        document.date.map(d => map.put(DateDP)(new Date(d.toEpochDay))).getOrElse(map)
      }
      else if(dp.name == AuthorDP.name) {
        document.sender.map(d => map.put(AuthorDP)(d)).getOrElse(map)
      }
      else if(dp.name == SubjectDP.name) {
        document.subject.map(d => map.put(SubjectDP)(d)).getOrElse(map)
      }
      else if(dp.name == TagListDP.name) {
        map.put(TagListDP)(document.tags)
      }
      else
      {
        map
      }
    }
    Doc(file.forward(id), pm)
  }
  private def docToDocumentId(doc: DocT): (IdT, Document) = (
    file.backward(doc.pdfFile),
    Document(
      sender = doc.properties.get(AuthorDP),
      subject = doc.properties.get(SubjectDP),
      tags = doc.properties.get(TagListDP).getOrElse(Set()),
      date = doc.properties.get(DateDP).map(_.toLocalDate)
    )
    )


  val internalUpdates: Subject[Unit] = Subject()

  val initialize: EitherT[IO, String, Unit] = for{
    _ <- backend.reloadDB
    _ <- backend.scanForPDFs
  } yield ()

  //load db initially
  initialize.value.unsafeRunSync()

  def docStream(reload: Observable[Unit] = Observable.just(())) : Observable[IndexedSeq[DocT]] = {
    (reload merge internalUpdates).map(_ => backend
      .getAllDocuments
      .value
      .unsafeRunSync()
      .getOrElse(Seq())
      .map((documentToDoc _).tupled)(collection.breakOut))
  }

  def persistable: Persistable[DocT] = (t: DocT) => {
    println(s"persisting ${t.pdfFile}")
    ( for{
      _ <- (backend.updateDocument _).tupled(docToDocumentId(t))
      _ <- EitherT.right[String](IO(internalUpdates.onNext(()))) //trigger update
    } yield () ).value.unsafeRunSync()
  }
}