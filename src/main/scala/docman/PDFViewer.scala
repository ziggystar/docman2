package docman

import resource._
import collection.JavaConverters._
import java.io.File
import scala.swing.{Button, Dimension, Component}
import java.awt.image.BufferedImage
import java.awt.Image
import org.apache.pdfbox.pdmodel.{PDPage, PDDocument}
import rx.core.{Obs, Rx}
import rx.Var
import java.util
import scala.collection.mutable
import javax.swing.{JButton, JLayeredPane}
import migpanel.MigPanel

/**
 * @author Thomas Geier
 * @since 12/15/13
 */
class PDFView(val pdfFile: Rx[Option[File]], val displayedPage: Rx[Int]) extends Component {
  minimumSize = new Dimension(300,400)

  val pageAsImage: Rx[Option[BufferedImage]] = Rx{
    pdfFile().flatMap(f => managed(PDDocument.load(f)).map(
      _.getDocumentCatalog
        .getAllPages.get(displayedPage())
        .asInstanceOf[PDPage]
        .convertToImage(BufferedImage.TYPE_BYTE_GRAY,300)
    ).opt)
  }

  val imageChanged = Obs(pageAsImage)(this.repaint())

  override protected def paintComponent(g: swing.Graphics2D) {
    g.clearRect(g.getClipBounds.x,g.getClipBounds.y,g.getClipBounds.width,g.getClipBounds.height)
    for(img <- pageAsImage()){
      val (imgW,imgH) = (img.getWidth, img.getHeight)
      val (cW,cH) = (this.peer.getWidth, this.peer.getHeight)
      val scale = math.min(cW / imgW.toDouble, cH / imgH.toDouble)
      g.drawImage(img.getScaledInstance((scale * imgW).toInt,(scale * imgH).toInt,Image.SCALE_DEFAULT),0,0,null)
    }
  }
}

object PDFViewer{
  def newViewer(file: Rx[Option[File]]) = {
    new MigPanel{
      val page = Var{0}
      val resetPage = Obs(file)(page.update(0))
      val nextPage = Button("->")(page.update(page() + 1))
      val prevPage = Button("<-")(page.update(page() - 1))
      val viewer = new PDFView(file, page)
      add(prevPage)
      add(nextPage, "wrap")
      add(viewer, "span 2, grow, push")
    }
  }

}