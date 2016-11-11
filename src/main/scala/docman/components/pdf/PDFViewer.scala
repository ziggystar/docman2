package docman.components.pdf

import java.awt.Image
import java.awt.image.BufferedImage
import java.io.File

import docman.components.{MigPanel, RxLabel}
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import rx.lang.scala._
import docman.utils.ResourceCache

import scala.concurrent.duration.Duration
import scala.swing.{Button, Component, Dimension}

/**
 * @author Thomas Geier
 * @since 12/15/13
 */
class PDFView(val pdfFile: Observable[Option[File]], val displayedPage: Observable[Int]) extends Component {
  minimumSize = new Dimension(300,400)

  private var file: Option[File] =  None
  private var page = 0
  //repaint if pdf or page changes
  pdfFile.combineLatest(pdfFile.map(_ => 0) merge displayedPage)
    .distinct
    .debounce(Duration("50ms")) //wait a bit until rendering the pdf
    .foreach{
    case (f,p) =>
      file = f
      page = p
      this.repaint()
  }

  override protected def paintComponent(g: swing.Graphics2D) {
    g.clearRect(g.getClipBounds.x,g.getClipBounds.y,g.getClipBounds.width,g.getClipBounds.height)
    file.foreach{f =>
      val img = PDFViewer.renderPDFPage(f,page)
      val (imgW,imgH) = (img.getWidth(this.peer), img.getHeight(this.peer))
      val (cW,cH) = (this.peer.getWidth, this.peer.getHeight)
      val scale = math.min(cW / imgW.toDouble, cH / imgH.toDouble)
      g.drawImage(img.getScaledInstance((scale * imgW).toInt,(scale * imgH).toInt,Image.SCALE_SMOOTH),0,0,null)
    }
  }
}

object PDFViewer{
  def newViewer(file: Observable[Option[File]]): MigPanel = {
    new MigPanel{
      val maxPage: Observable[Int] = file.map(_.map(getNumPages).getOrElse(0)).cache
      val nextPage: Subject[Unit] = Subject[Unit]()
      val prevPage: Subject[Unit] = Subject[Unit]()
      val nextPageButton: Button = Button("→")(nextPage.onNext(()))
      val prevPageButton: Button = Button("←")(prevPage.onNext(()))
      val page: Observable[Int] =
        file
          .map(_ => (nextPage.map(_ => +1) merge prevPage.map(_ => -1))
            .scan(0)(_ + _)
              .combineLatestWith(maxPage){case (p,max) => if(p < 0) 0 else if(p > max - 1) max - 1 else p})
          .switch
      val pageLabel = new RxLabel(page.combineLatest(maxPage).map{case (p,max) => s"${p+1}/$max"})
      val viewer = new PDFView(file, page)
      add(prevPageButton, "split 3")
      add(pageLabel)
      add(nextPageButton, "wrap")
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