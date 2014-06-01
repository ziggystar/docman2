package docman

import resource._
import java.io.File
import scala.swing.{Button, Dimension, Component}
import java.awt.image.BufferedImage
import java.awt.Image
import org.apache.pdfbox.pdmodel.{PDPage, PDDocument}
import rx._
import rx.ops._
import migpanel.MigPanel
import rxutils.swing.RxLabel

/**
 * @author Thomas Geier
 * @since 12/15/13
 */
class PDFView(val pdfFile: Rx[Option[File]], val displayedPage: Rx[Int]) extends Component {
  minimumSize = new Dimension(300,400)

  val pageAsImage: Rx[Option[Image]] = Rx{pdfFile().map(PDFViewer.renderPDFPage(_,displayedPage()))}

  val imageChanged = Obs(pageAsImage)(this.repaint())

  override protected def paintComponent(g: swing.Graphics2D) {
    g.clearRect(g.getClipBounds.x,g.getClipBounds.y,g.getClipBounds.width,g.getClipBounds.height)
    for(img <- pageAsImage()){
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

  def getNumPages(file: File): Int =
    managed(PDDocument.load(file)).map(pd =>
      pd.getNumberOfPages
    ).opt.getOrElse(0)

  def renderPDFPage(file: File, page: Int, dpi: Int = 150): Image = {
    managed(PDDocument.load(file)).map(pd =>
      pd.getDocumentCatalog.getAllPages.get(page).asInstanceOf[PDPage].convertToImage(BufferedImage.TYPE_BYTE_GRAY, dpi)
    ).opt.getOrElse(new BufferedImage(1,1,BufferedImage.TYPE_BYTE_GRAY))
  }
}