package docman.frontend.swing2

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.image.BufferedImage
import java.awt.{Component, Graphics, Image}
import java.io.File

import cats.effect._
import cats.syntax.applicative.catsSyntaxApplicativeId
import com.typesafe.scalalogging.StrictLogging
import javax.swing.{JButton, JLabel, JPanel, JToolBar}
import jiconfont.icons.font_awesome.FontAwesome
import jiconfont.swing.IconFontSwing
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.Atomic
import monix.execution.{Ack, Cancelable}
import monix.reactive.{Observable, Observer}
import net.miginfocom.swing.MigLayout
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}

package object components extends StrictLogging {

  IconFontSwing.register(FontAwesome.getIconFont)

  def loadPDF(pdfFile: Observable[Option[File]]): Observable[Option[PDDocument]] =
    pdfFile.flatMap(of => Observable.resource(Task {
      logger.info(s"loading pdf from $of")
      of.map(PDDocument.load)
    })(pd => Task(pd.foreach(_.close()))))
    .doOnNext(of => Task(logger.debug(s"loading new PDDocument")))

  def rxpdfview[F[_] : Sync](pdf: Observable[Option[File]]): Resource[F, Component] = for {
    toolbar <- new JToolBar("pdf-control").pure[Resource[F, *]]
      .evalTap(tb => Sync[F].delay {
        tb.setFloatable(false)
      })

    pdfpanel <- Resource.make(Sync[F].delay{
      new JPanel(true) { outer =>
        var currentPage: Atomic[Option[BufferedImage]] = Atomic(Option.empty[BufferedImage])

        val subscription: Cancelable = loadPDF(pdf).doOnNext(of =>
          Task{
            currentPage := of.map{pd => new PDFRenderer(pd).renderImageWithDPI(0, 90, ImageType.RGB)}
            logger.debug(s"rendered pdf (${currentPage.get().map(bi => (bi.getWidth, bi.getHeight()))}")
            outer.repaint()
          }).subscribe()

        override protected def paintComponent(g: Graphics) {
          g.clearRect(g.getClipBounds.x, g.getClipBounds.y, g.getClipBounds.width, g.getClipBounds.height)
          currentPage.get.foreach { img =>
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

  def rxlabel[F[_] : Sync](text: Observable[String]): Resource[F, Component] = Resource.make(
    Sync[F].delay {
      val c = new JLabel("")
      val canc = text.doOnNextF(s => Task {
        c.setText(s)
      }).subscribe()
      (c, canc)
    })(x => Sync[F].delay {
    x._2.cancel()
  }).map(_._1)

  def rxtags[F[_] : Sync](currentTags: Observable[Set[String]], selection: Observer[Set[String]]): Resource[F, Component] = ???

  def rxtextfield[F[_] : Effect](content: Observer[String]): Resource[F, Component] = ???

  def rxbutton[F[_] : Sync](label: Observable[String], clicks: Observer[Unit]): Resource[F, JButton] = Resource.make(
    Sync[F].delay {
      val button = new JButton("foo")
      val labelCancel = label.doOnNext(newLabel => Task(button.setText(newLabel))).subscribe()
      //ignore Future[Ack]
      val eventToUnit: ActionListener = new ActionListener {
        al =>
        override def actionPerformed(e: ActionEvent): Unit = {
          clicks.onNext()
            //unregister action listener
            .filter(_ == Ack.Stop).foreach(_ => button.removeActionListener(al))
        }
      }
      button.addActionListener(eventToUnit)
      (button, labelCancel, eventToUnit)
    }
  )({
    case (b, c, e) => Sync[F].delay {
      logger.info("unregistering button")
      //cancel subscription
      c.cancel()
      //terminate clicks
      b.removeActionListener(e)
      clicks.onComplete()
    }
  })
    .map(_._1) //extract button
}
