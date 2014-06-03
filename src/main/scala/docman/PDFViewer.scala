package docman

import java.io.File
import scala.swing.{Button, Dimension, Component}
import java.awt.image.BufferedImage
import java.awt.Image
import org.apache.pdfbox.pdmodel.{PDPage, PDDocument}
import rx._
import migpanel.MigPanel
import rxutils.swing.RxLabel
import utils.ResourceCache

/**
 * @author Thomas Geier
 * @since 12/15/13
 */
class PDFView(val pdfFile: Rx[Option[File]], val displayedPage: Rx[Int]) extends Component {
  minimumSize = new Dimension(300,400)

  val pdfAndPage = Rx{(pdfFile(),displayedPage())}
  val imageChanged = Obs(pdfAndPage)(this.repaint())

  override protected def paintComponent(g: swing.Graphics2D) {
    g.clearRect(g.getClipBounds.x,g.getClipBounds.y,g.getClipBounds.width,g.getClipBounds.height)
    for(img <- pdfFile().map(PDFViewer.renderPDFPage(_,displayedPage()))){
      val (imgW,imgH) = (img.getWidth(this.peer), img.getHeight(this.peer))
      val (cW,cH) = (this.peer.getWidth, this.peer.getHeight)
      val scale = math.min(cW / imgW.toDouble, cH / imgH.toDouble)
      g.drawImage(img.getScaledInstance((scale * imgW).toInt,(scale * imgH).toInt,Image.SCALE_SMOOTH),0,0,null)
    }
  }
}

object PDFViewer{
  def newViewer(file: Rx[Option[File]]) = {
    new MigPanel{
      val maxPage = Rx{file().fold(0)(getNumPages)}
      val page = Var{0}
      val resetPage = Obs(file)(page.update(0))
      val nextPage = Button("→")(if(page() < maxPage() - 1) page.update(page() + 1))
      val prevPage = Button("←")(if(page() > 0) page.update(page() - 1))
      val pageLabel = new RxLabel(Rx{s"${page() + 1}/${maxPage()}"})
      val statusNext = Obs(page)(nextPage.enabled = page() < maxPage() - 1)
      val statusPrev = Obs(page)(prevPage.enabled = page() > 0)
      val viewer = new PDFView(file, page)
      add(prevPage, "split 3")
      add(pageLabel)
      add(nextPage, "wrap")
      add(viewer, "grow, push")
    }
  }

  private val documentCache: ResourceCache[File, PDDocument] = new ResourceCache(PDDocument.load(_: File))

  def getNumPages(file: File): Int = documentCache.get(file)(_.getNumberOfPages).getOrElse(0)

  def renderPDFPage(file: File, page: Int, dpi: Int = 150): Image = {
    documentCache.get(file)(pd =>
      pd.getDocumentCatalog.getAllPages.get(page).asInstanceOf[PDPage].convertToImage(BufferedImage.TYPE_BYTE_GRAY, dpi)
    ).getOrElse(new BufferedImage(1,1,BufferedImage.TYPE_BYTE_GRAY))
  }
}