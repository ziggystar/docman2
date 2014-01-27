package docman

import resource._
import collection.JavaConverters._
import java.io.File
import scala.swing.{Dimension, Component}
import java.awt.image.BufferedImage
import java.awt.Image
import org.apache.pdfbox.pdmodel.{PDPage, PDDocument}

/**
 * @author Thomas Geier
 * @since 12/15/13
 */
class PDFViewer(var pdf: Option[File] = None) extends Component {
  minimumSize = new Dimension(400,500)

  def getPageAsImage(f: File, page: Int, resolution: Int = 200): Either[String,BufferedImage] = (for{
    pd <- managed(PDDocument.load(f))
  } yield pd.getDocumentCatalog.getAllPages.asScala(page).asInstanceOf[PDPage]
      .convertToImage(BufferedImage.TYPE_BYTE_GRAY,resolution)).either.left.map(_.toString)


  /** Tries to load the specified page of the currently set pdf file. */
  def getPage(page: Int, resolution: Int = 200): Option[BufferedImage] = (for{
    file <- pdf.toRight("no image set").right
    img <- getPageAsImage(file,page, resolution).right
  } yield img).right.toOption

  def setFile(f: Option[File]){
    this.pdf = f
    repaint()
  }

  override protected def paintComponent(g: swing.Graphics2D) {
    g.clearRect(g.getClipBounds.x,g.getClipBounds.y,g.getClipBounds.width,g.getClipBounds.height)
    for(img <- getPage(0)){
      val (imgW,imgH) = (img.getWidth, img.getHeight)
      val (cW,cH) = (this.peer.getWidth, this.peer.getHeight)
      val scale = math.min(cW / imgW.toDouble, cH / imgH.toDouble)
      g.drawImage(img.getScaledInstance((scale * imgW).toInt,(scale * imgH).toInt,Image.SCALE_DEFAULT),0,0,null)
    }
  }
}