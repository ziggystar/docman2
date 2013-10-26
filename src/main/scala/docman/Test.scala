package docman

import java.io.{FilenameFilter, File}
import resource._
import org.apache.pdfbox.pdmodel.{PDPage, PDDocument}
import collection.JavaConverters._
import scala.swing._
import java.awt.image.BufferedImage
import java.awt.Image
import scala.swing.event.TableRowsSelected
import javax.swing.ListSelectionModel

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

object Test extends Reactor {
  val dir = new File("documents/A")
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
      peer.setAutoCreateRowSorter(true)
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



