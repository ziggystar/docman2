package docman.components.pdf

import java.awt.Image
import java.awt.image.BufferedImage
import java.io.File

import com.typesafe.scalalogging.StrictLogging
import docman.components.{MigPanel, RxLabel}
import org.apache.pdfbox.pdmodel.{PDDocument, PDPage}
import rx.lang.scala._
import docman.utils.ResourceCache
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}

import scala.concurrent.duration.Duration
import scala.swing.{Button, Component, Dimension}

/**
 * @author Thomas Geier
 * @since 12/15/13
 */
class PDFView(val pdfFile: Observable[Option[File]], val displayedPage: Observable[Int]) extends Component with StrictLogging {
  minimumSize = new Dimension(300,400)

  private var file: Option[File] =  None
  private var page = 0
  //repaint if pdf or page changes
  pdfFile.combineLatest(displayedPage)
    .distinctUntilChanged
    .debounce(Duration("50ms")) //wait a bit until rendering the pdf
    .foreach{
    case (f,p) =>
      file = f
      page = p - 1
      logger.debug(s"changing pdf page to file $f page $p")
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

object PDFViewer extends StrictLogging {
  def newViewer(file: Observable[Option[File]]): MigPanel = {
    file.foreach(f => logger.debug(s"changing file to $f"))
    new MigPanel{
      val maxPage: Observable[Int] = file.map(_.map(getNumPages).getOrElse(0))
      val nextPage: Subject[Unit] = Subject[Unit]()
      val prevPage: Subject[Unit] = Subject[Unit]()
      val nextPageButton: Button = Button("→")(nextPage.onNext(()))
      val prevPageButton: Button = Button("←")(prevPage.onNext(()))
      val turnPage: Observable[Int] = nextPage.map(_ => +1) merge prevPage.map(_ => -1)
      val page: Observable[Int] =
        maxPage.switchMap(max =>
          turnPage.scan(1){case (current,diff) => math.max(1,math.min(max,current + diff))}.distinctUntilChanged
        )
      val pageLabel = new RxLabel(page.combineLatest(maxPage).map{case (p,max) => s"$p/$max"})
      val viewer = new PDFView(file, page)
      add(prevPageButton, "split 3")
      add(pageLabel)
      add(nextPageButton, "wrap")
      add(viewer, "grow, push")
    }
  }

  private val documentCache: ResourceCache[File, PDDocument] = new ResourceCache(PDDocument.load(_: File))

  def getNumPages(file: File): Int = documentCache.get(file)(_.getNumberOfPages).getOrElse(0)

  def renderPDFPage(file: File, page: Int, dpi: Int = 90): Image = {
    documentCache.get(file) { pd =>
      new PDFRenderer(pd).renderImageWithDPI(page,dpi,ImageType.RGB)
    }.getOrElse(new BufferedImage(1,1,BufferedImage.TYPE_BYTE_GRAY))
  }
}