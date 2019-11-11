package docman.frontend.swing2

import java.awt.event.{ActionEvent, ActionListener}
import java.nio.file.Path

import cats.effect._
import cats.effect.syntax.async
import com.typesafe.scalalogging.StrictLogging
import javax.swing.JButton
import monix.eval.Task
import monix.execution.Ack
import monix.reactive.{Observable, Observer}
import monix.execution.Scheduler.Implicits.global

import scala.swing.Component

package object components extends StrictLogging {

  def rxpdfview[F[_]: Effect](pdf: Observable[Option[Path]]): Resource[F, Component] = ???

  def rxtags[F[_]: Effect](currentTags: Observable[Set[String]], selection: Observer[Set[String]]): Resource[F, Component] = ???

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
