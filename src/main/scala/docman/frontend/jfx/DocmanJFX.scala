package docman.frontend.jfx

import java.io.File
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.data.EitherT
import cats.effect.IO
import docman.core.{Document, RODocumentStore}
import docman.engine.SideCarRO
import docman.utils.ResourceCache
import docman.utils.jfx._
import javafx.application.Application
import javafx.beans.property.{ReadOnlyObjectProperty, ReadOnlyObjectWrapper}
import javafx.embed.swing.SwingFXUtils
import javafx.scene.{Node, Scene}
import javafx.scene.control.cell.TextFieldTableCell
import javafx.scene.control.{TableColumn, TableView}
import javafx.scene.image.{Image, ImageView, WritableImage}
import javafx.scene.paint.Color
import javafx.stage.Stage
import javafx.util.StringConverter
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}

import scala.collection.JavaConverters._

class DocmanJFX extends Application {
  override def start(primaryStage: Stage): Unit = {

    val backend = SideCarRO(Seq(new File("documents") -> true))

    val table = DocTable(backend)

    val scene = new Scene(table, 400, 300)

    primaryStage.setTitle("My JavaFX Application")
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}

object DocmanJFX {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[DocmanJFX], args: _*)
  }
}

object PDFView {
  private val documentCache: ResourceCache[File, PDDocument] = new ResourceCache(PDDocument.load(_: File))
  def makePDFView(p: ReadOnlyObjectProperty[Option[File]]): Node = {
    val imageView = new ImageView()
    imageView.imageProperty().bind(p.map(of =>
      of
        .map(f => documentCache.get(f)(
            pd => new PDFRenderer(pd).renderImageWithDPI(0,100,ImageType.RGB)
          ))
        .flatMap(_.toOption)
        .map{bi =>
          val fxImage = new WritableImage(600,800)
          SwingFXUtils.toFXImage(bi, fxImage)
        }
        .getOrElse(new WritableImage(600,800))
    ))
    imageView
  }
}
object DocTable {
  type Row = (File, Document)

  def tableColumnOpt[A,B](title: String, f: A => Option[B], to: B => String, from: String => B): TableColumn[A,Option[B]] = {
    val tc = new TableColumn[A,Option[B]](title)
    tc.setCellValueFactory(a => new ReadOnlyObjectWrapper(f(a.getValue)))
    tc.setCellFactory{ a =>
      val sc: StringConverter[Option[B]] = new StringConverter[Option[B]] {
        override def toString(o: Option[B]): String = o.map(to).getOrElse("–")
        override def fromString(string: String): Option[B] = if(string == "–") None else Some(from(string))
      }
      val cell = new TextFieldTableCell[A,Option[B]](sc){
        override def updateItem(item: Option[B], empty: Boolean): Unit = {
          super.updateItem(item, empty)
          this.setTextFill(Option(item).flatten.fold(Color.DARKGRAY)(_ => Color.BLACK))
        }
      }
      cell
    }
    tc.setEditable(true)
    tc
  }

  def tableColumn[A,B](title: String, f: A => B): TableColumn[A,B] = {
    val tc = new TableColumn[A,B](title)
    tc.setCellValueFactory(a => new ReadOnlyObjectWrapper(f(a.getValue)))
    tc
  }

  def apply(db: RODocumentStore[EitherT[IO,String,?]]{type Id = File}): TableView[Row] = {
    val table = new TableView[Row]()

    table.setEditable(true)

    table.getColumns.addAll(
      tableColumn[Row,String]("File", _._1.toString),
      tableColumnOpt[Row,String]("Sender", _._2.sender, identity, identity),
      tableColumnOpt[Row,String]("Subject", _._2.subject, identity, identity),
      tableColumnOpt[Row,LocalDate]("Date", _._2.date, _.format(DateTimeFormatter.ISO_DATE), s => LocalDate.parse(s))
    )

    val docs: Seq[(File, Document)] = db.getAllDocuments.value.unsafeRunSync().fold(_ => Seq(), identity)

    table.getItems.addAll(docs.asJava)

    table
  }
}
