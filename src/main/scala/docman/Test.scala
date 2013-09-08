package docman

import java.io.{FileWriter, FilenameFilter, File}
import resource._
import org.apache.pdfbox.pdmodel.{PDPage, PDDocumentInformation, PDDocument}
import collection.JavaConverters._
import scala.swing._
import javax.swing.table.{DefaultTableCellRenderer, AbstractTableModel}
import java.awt.image.BufferedImage
import java.awt.Image
import scala.swing.event.TableRowsSelected
import javax.swing.{JTextField, DefaultCellEditor, ListSelectionModel}
import java.text.SimpleDateFormat
import scala.collection.mutable
import scala.io.Source

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

object Test extends Reactor {
  val dir = new File("/home/thomas/Dokumente/A")
  val pdfs: Array[File] =
    dir.listFiles(new FilenameFilter { def accept(dir: File, name: String): Boolean = name.endsWith("pdf")})


  def getFirstPageAsImage(f: File): BufferedImage = (for(pd <- managed(PDDocument.load(f))) yield
    pd.getDocumentCatalog.getAllPages.asScala.head.asInstanceOf[PDPage].convertToImage(BufferedImage.TYPE_BYTE_GRAY,150)).opt.get

  def main(args: Array[String]) {
    val docs = pdfs.map(Doc.fromFile)
    val tableModel = TModel(docs, DocProperty.ALL)

    val table = new Table{
      model = tableModel
      peer.getSelectionModel.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
      (0 until peer.getColumnCount).map(peer.getColumnModel.getColumn).zip(DocProperty.ALL).foreach{case (colModel, prop) =>
        colModel.setCellEditor(prop.cellEditor)
        colModel.setCellRenderer(prop.cellRenderer)
      }
      def openSelectedPDF() {
        val selectedPDF = selection.rows.head
        sys.process.Process(f"gnome-open ${tableModel.getDocAtRow(selectedPDF).pdfFile.getAbsoluteFile}").run()
      }
    }

    val viewer = new ImageViewer(Some(getFirstPageAsImage(pdfs(0))))

    this.listenTo(table.selection)
    this.reactions += {
      case TableRowsSelected(_,_,_) => viewer.setNewImage(getFirstPageAsImage(pdfs(table.selection.rows.head)))
    }

    val menuB = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Save All Meta"){tableModel.saveAllMeta()})
        contents += new MenuItem(Action("Open Selected PDF"){table.openSelectedPDF()})
      }
    }

    val frame = new MainFrame{
      menuBar = menuB
      minimumSize = new Dimension(800,600)
      contents = new SplitPane(Orientation.Vertical,new ScrollPane(table),viewer)
    }
    viewer.listenTo(table)

    frame.open()
  }
}

/**
 * @param pdfFile
 * @param sidecar Sidecar file. Does not necessarily exist.
 * @param properties
 */
case class Doc(pdfFile: File, sidecar: File, properties: Map[DocProperty,String])
object Doc{
  def getMetaData(f: File): PDDocumentInformation =
    (for(pd <- managed(PDDocument.load(f))) yield pd.getDocumentInformation).opt.get
  def fromFile(pdf: File): Doc = {
    val sidecar = new File(pdf.getAbsolutePath.dropRight(3) + "smd")
    val pdfMeta = getMetaData(pdf)
    val mapFromPdf: Map[DocProperty, String] = DocProperty.ALL.foldLeft(Map[DocProperty,String]()){case (map, prop) =>
      map + (prop -> prop.pdfboxExtractor(pdfMeta))
    }
    val mapFromSidecar: Map[DocProperty, String] = Option(sidecar).filter(_.exists).map(sc =>
      ( for(src <- managed(Source.fromFile(sc))) yield {
        (for{
          line <- src.getLines()
          prop <- DocProperty.ALL
        } yield prop.deserialize(line).map(prop -> _)).flatten.toMap
      }).opt).flatten.getOrElse(Map())
    Doc(pdf,sidecar,mapFromPdf ++ mapFromSidecar)
  }

  /** @return The updated Doc object. */
  def saveMeta(d: Doc) {
    for(writer <- managed(new FileWriter(d.sidecar))) {
      d.properties.map{case (prop, value) => prop.serializeToLine(value)}.foreach{line =>
        writer.write(line + "\n")
      }
    }
  }
}

class ImageViewer(var image: Option[BufferedImage] = None) extends Component {

  minimumSize = new Dimension(400,500)
  def setNewImage(img: BufferedImage){
    this.image = Some(img)
    peer.repaint()
  }

  override protected def paintComponent(g: swing.Graphics2D) {
    g.clearRect(g.getClipBounds.x,g.getClipBounds.y,g.getClipBounds.width,g.getClipBounds.height)
    for(img <- image){
      g.drawImage(img.getScaledInstance(peer.getWidth,peer.getHeight,Image.SCALE_DEFAULT),0,0,null)
    }
  }
}

case class TModel(_docs: IndexedSeq[Doc], properties: IndexedSeq[DocProperty]) extends AbstractTableModel{
  var docs = mutable.ArrayBuffer(_docs:_*)

  val updates = new mutable.HashMap[Int,Map[Int,String]]

  override def getColumnName(column: Int): String = column match {
    case 0 => "Edited"
    case 1 => "File"
    case x => properties(x - 2).name
  }

  def getRowCount: Int = docs.size
  def getColumnCount: Int = properties.size + 2
  def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = columnIndex match {
    case 0 => if(updates.isDefinedAt(rowIndex)) "+" else ""
    case 1 => docs(rowIndex).pdfFile.getName
    case c => updates.get(rowIndex).flatMap(_.get(columnIndex)).getOrElse(docs(rowIndex).properties(properties(c - 2)))
  }


  override def setValueAt(aValue: AnyRef, rowIndex: Int, columnIndex: Int) {
    updates.update(rowIndex, updates.getOrElse(rowIndex,Map()) + (columnIndex -> aValue.toString))
  }

  override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = columnIndex > 1

  def saveAllMeta() {
    updates.foreach{case (rowIdx, updates) =>
      val doc = docs(rowIdx)
      val docUpdates = updates.map{case (colIdx,value) =>
        properties(colIdx - 2) -> value
      }
      val newDoc = doc.copy(properties = doc.properties ++ docUpdates)
      Doc.saveMeta(newDoc)
      docs(rowIdx) = newDoc
    }
    updates.clear()
  }

  def getDocAtRow(row: Int) = docs(row)
}

trait DocProperty {
  def cellRenderer = new DefaultTableCellRenderer
  def cellEditor = new DefaultCellEditor(new JTextField)
  def isEditable: Boolean = true
  def name: String
  def defaultValue: String
  def pdfboxExtractor: PDDocumentInformation => String
  def serialPrefix: String
  def serializeToLine(a: String): String = f"$serialPrefix:$a"
  def deserialize(line: String): Option[String] =
    Option(line).filter(_.startsWith(f"$serialPrefix:")).map(_.drop(serialPrefix.size + 1))
}

object DocProperty {
  val ALL = IndexedSeq(AuthorProp, SubjectProp, DateProp)
}

object AuthorProp extends DocProperty{
  def name: String = "Author"
  def defaultValue: String = ""
  def pdfboxExtractor: (PDDocumentInformation) => String =  di => di.getAuthor
  def serialPrefix: String = "AUTHOR"
}

object SubjectProp extends DocProperty{
  def name: String = "Subject"
  def defaultValue: String = ""
  def pdfboxExtractor: (PDDocumentInformation) => String =  di => di.getSubject
  def serialPrefix: String = "SUBJECT"
}

object DateProp extends DocProperty{
  override def cellRenderer = new DefaultTableCellRenderer {
    val f = new SimpleDateFormat("MM/dd/yy")

    override def setValue(value: scala.Any) {
      setText(if(value == null) "" else f.format(value))
    }
  }

  def name: String = "Date"

  def defaultValue: String = "1.1.2013"

  def pdfboxExtractor: (PDDocumentInformation) => String = di => ""

  def serialPrefix: String = "Date"
}