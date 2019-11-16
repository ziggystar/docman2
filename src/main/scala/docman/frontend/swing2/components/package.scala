package docman.frontend.swing2

import java.awt.{Component, Graphics2D, Image}
import java.awt.event.{ActionEvent, ActionListener}
import java.nio.file.Path

import cats.effect._
import cats.syntax.applicative.catsSyntaxApplicativeId
import com.typesafe.scalalogging.StrictLogging
import javax.swing.{JButton, JLabel, JPanel, JToolBar}
import jiconfont.icons.font_awesome.FontAwesome
import jiconfont.swing.IconFontSwing
import monix.eval.Task
import monix.execution.Ack
import monix.reactive.{Observable, Observer}
import monix.execution.Scheduler.Implicits.global
import net.miginfocom.swing.MigLayout
import org.apache.pdfbox.rendering.{ImageType, PDFRenderer}

package object components extends StrictLogging {
  IconFontSwing.register(FontAwesome.getIconFont)

  def rxpdfview[F[_]: Sync](pdf: Observable[Option[Path]]): Resource[F, Component] = for {
    panel <- new JPanel(new MigLayout).pure[Resource[F,*]]
    _ <- Resource.liftF(Sync[F].delay {
      val tb = new JToolBar()
      tb.setFloatable(false)
      Seq(FontAwesome.ANGLE_DOUBLE_LEFT, FontAwesome.ANGLE_LEFT, FontAwesome.ANGLE_RIGHT, FontAwesome.ANGLE_DOUBLE_RIGHT)
        .map(i => new JButton(IconFontSwing.buildIcon(i,16)))
        .foreach(tb.add)
      panel.add(tb, "pushx, growx, wrap")

      val pane = new JPanel(true){
        override protected def paintComponent(g: Graphics2D) {
          g.clearRect(g.getClipBounds.x,g.getClipBounds.y,g.getClipBounds.width,g.getClipBounds.height)
          file.foreach{f =>
            val img = new PDFRenderer(f).renderImageWithDPI(page,dpi,ImageType.RGB)
            val (imgW,imgH) = (img.getWidth(this.peer), img.getHeight(this.peer))
            val (cW,cH) = (this.peer.getWidth, this.peer.getHeight)
            val scale = math.min(cW / imgW.toDouble, cH / imgH.toDouble)
            g.drawImage(img.getScaledInstance((scale * imgW).toInt,(scale * imgH).toInt,Image.SCALE_SMOOTH),0,0,null)
          }
      }
      panel.add(pane)
    })
  } yield panel

  def rxlabel[F[_]: Sync](text: Observable[String]): Resource[F,Component] = Resource.make(
    Sync[F].delay {
      val c = new JLabel("")
      val canc = text.doOnNextF(s => Task{c.setText(s)}).subscribe()
      (c,canc)
    })(x => Sync[F].delay {
    x._2.cancel()
  }).map(_._1)

  def rxtags[F[_]: Sync](currentTags: Observable[Set[String]], selection: Observer[Set[String]]): Resource[F, Component] = ???

  def rxtextfield[F[_]: Effect](content: Observer[String]): Resource[F,Component] = ???

  def rxbutton[F[_]: Sync](label: Observable[String], clicks: Observer[Unit]): Resource[F,JButton] = Resource.make(
    Sync[F].delay{
      val button = new JButton("foo")
      val labelCancel = label.doOnNext(newLabel => Task(button.setText(newLabel))).subscribe()
      //ignore Future[Ack]
      val eventToUnit: ActionListener = new ActionListener {al =>
        override def actionPerformed(e: ActionEvent): Unit = {
          clicks.onNext()
            //unregister action listener
            .filter(_ == Ack.Stop).foreach(_ => button.removeActionListener(al))
        }
      }
      button.addActionListener(eventToUnit)
      (button,labelCancel,eventToUnit)
    }
  )({
    case (b,c,e) => Sync[F].delay{
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
