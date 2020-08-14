package docman.frontend.swing2.components

import java.awt.image.BufferedImage
import java.awt.{Component, Graphics, Image}
import java.io.File

import cats.effect._
import cats.implicits._
import docman.utils.Logging
import javax.swing.{JPanel, JToolBar}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import net.miginfocom.swing.MigLayout
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}

//better refactor the code
import scala.language.reflectiveCalls

case object pdfview extends Logging {
  def loadPDF(pdfFile: Observable[Option[File]]): Observable[Option[PDDocument]] =
    pdfFile.flatMap(of => Observable.resource(Task{
      logger.info(s"loading pdf from $of")
      of.map(PDDocument.load)
    }.executeAsync)(pd => Task(pd.foreach(_.close()))))
      .doOnNext(of => Task(logger.debug(s"loading new PDDocument")))

  def apply[F[_] : Sync](pdf: Observable[Option[File]]): Resource[F, Component] = for {
    toolbar <- new JToolBar("pdf-control").pure[Resource[F, *]]
      .evalTap(tb => Sync[F].delay {
        tb.setFloatable(false)
      })

    pdfpanel <- Resource.make(Sync[F].delay{
      new JPanel(true) { outer =>
        var currentPage: Atomic[Option[BufferedImage]] = Atomic(Option.empty[BufferedImage])

        val subscription = loadPDF(pdf).doOnNext(of =>
          Task{
            currentPage := of.map{pd => new PDFRenderer(pd).renderImageWithDPI(0, 90, ImageType.RGB)}
            logger.debug(s"rendered pdf (${currentPage.get().map(bi => (bi.getWidth, bi.getHeight()))}")
            outer.repaint()
          }.executeAsync).subscribe()

        override protected def paintComponent(g: Graphics): Unit = {
          g.clearRect(g.getClipBounds.x, g.getClipBounds.y, g.getClipBounds.width, g.getClipBounds.height)
          currentPage.get().foreach { img =>
            val (imgW, imgH) = (img.getWidth(this), img.getHeight(this))
            logger.debug(s"drawing pdf on screen ($imgW x $imgH)")
            val (cW, cH) = (this.getWidth, this.getHeight)
            val scale = math.min(cW / imgW.toDouble, cH / imgH.toDouble)
            g.drawImage(img.getScaledInstance((scale * imgW).toInt, (scale * imgH).toInt, Image.SCALE_SMOOTH), 0, 0, null)
          }
        }
      }
    })(jp => Sync[F].delay(jp.subscription.cancel()))

    panel <- new JPanel(new MigLayout).pure[Resource[F, *]]
      .evalTap(panel => Sync[F].delay {
        panel.add(toolbar, "grow x, wrap")
      })
      .evalTap(panel => Sync[F].delay {
        panel.add(pdfpanel, "grow, push")
      })
  } yield panel
}
