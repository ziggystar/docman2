package docman

import java.io.{FilenameFilter, File}
import resource._
import org.apache.pdfbox.pdmodel.{PDPage, PDDocumentInformation, PDDocument}
import collection.JavaConverters._
import scala.swing._
import javax.swing.table.{AbstractTableModel}
import migpanel.MigPanel
import java.awt.image.{RescaleOp, BufferedImage}

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

object Test {
  val dir = new File("/home/thomas/Dokumente/A")
  val pdfs: Array[File] =
    dir.listFiles(new FilenameFilter { def accept(dir: File, name: String): Boolean = name.endsWith("pdf")})

  def getMetaData(f: File): PDDocumentInformation =
    (for(pd <- managed(PDDocument.load(f))) yield pd.getDocumentInformation).opt.get
  def getFirstPageAsImage(f: File) = (for(pd <- managed(PDDocument.load(f))) yield
    pd.getDocumentCatalog.getAllPages.asScala.head.asInstanceOf[PDPage].convertToImage(BufferedImage.TYPE_BYTE_GRAY,90)).opt.get

  def main(args: Array[String]) {
    val infos = pdfs.map(getMetaData)
    val tableModel = TModel(infos,IndexedSeq(AuthorProp,SubjectProp))
    val table = new Table{
      model = tableModel
    }

    val viewer = new ImageViewer(Some(getFirstPageAsImage(pdfs(0))))

    val panel = new MigPanel
    val frame = new MainFrame{
      minimumSize = new Dimension(800,600)
      contents = panel
    }
    panel.add(new ScrollPane(table),"growy")
    panel.add(viewer, "growx")

    frame.open()
  }
}

class ImageViewer(var image: Option[BufferedImage] = None) extends Component {
  minimumSize = new Dimension(400,500)
  def setNewImage(img: BufferedImage){
    this.image = image
  }

  override protected def paintComponent(g: swing.Graphics2D) {
    g.clearRect(g.getClipBounds.x,g.getClipBounds.y,g.getClipBounds.width,g.getClipBounds.height)
    for(img <- image){
      g.drawImage(img,new RescaleOp(1f,0f,null),0,0)
    }
  }
}

case class TModel(docs: IndexedSeq[PDDocumentInformation], properties: IndexedSeq[DocProperty]) extends AbstractTableModel{

  override def getColumnName(column: Int): String = properties(column).name

  def getRowCount: Int = docs.size
  def getColumnCount: Int = properties.size
  def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = properties(columnIndex).pdfboxExtractor(docs(rowIndex))
}

trait DocProperty {
  def name: String
  def defaultValue: AnyRef
  def pdfboxExtractor: PDDocumentInformation => AnyRef
  def serialPrefix: String
  def serializeToLine(a: AnyRef): String = f"$serialPrefix:$a"
  def deserialize(line: String): Option[AnyRef] =
    Option(line).filter(_.startsWith(f"$serialPrefix:")).map(_.drop(serialPrefix.size + 1))
}

object AuthorProp extends DocProperty{
  def name: String = "Author"
  def defaultValue: AnyRef = ""
  def pdfboxExtractor: (PDDocumentInformation) => AnyRef =  di => di.getAuthor
  def serialPrefix: String = "AUTHOR"
}

object SubjectProp extends DocProperty{
  def name: String = "Subject"
  def defaultValue: AnyRef = ""
  def pdfboxExtractor: (PDDocumentInformation) => AnyRef =  di => di.getSubject
  def serialPrefix: String = "SUBJECT"
}