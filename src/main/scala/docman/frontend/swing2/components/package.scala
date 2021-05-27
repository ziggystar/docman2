package docman.frontend.swing2

import java.awt.Component
import java.awt.event.{ActionEvent, ActionListener}

import cats.effect._
import docman.utils.Logging
import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.{JButton, JLabel, JTextField}
import jiconfont.icons.font_awesome.FontAwesome
import jiconfont.swing.IconFontSwing
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.{Ack, Cancelable}
import monix.reactive.{Observable, Observer}

package object components extends Logging {

  IconFontSwing.register(FontAwesome.getIconFont)

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

  def rxtextfield[F[_] : Effect](content: Observer[String]): Resource[F, Component] = Resource.make(
    Sync[F].delay {
      val tf = new JTextField("")
      //ignore Future[Ack]
      val listener = new DocumentListener {
        def stupidJava(e: DocumentEvent): Unit = { content.onNext(tf.getText) }
        override def insertUpdate(e: DocumentEvent): Unit = stupidJava(e)
        override def removeUpdate(e: DocumentEvent): Unit = stupidJava(e)
        override def changedUpdate(e: DocumentEvent): Unit = stupidJava(e)
      }
      logger.debug("adding listener to textfield")
      tf.getDocument.addDocumentListener(listener)
      (tf, listener)
    }
  )({
    case (tf,l) =>
      Sync[F].delay{
        logger.debug("removing listener from text field")
        tf.getDocument.removeDocumentListener(l)
      }
  }).map(_._1)

  def rxbutton[F[_] : Sync](label: Observable[String], clicks: Observer[Unit], enabled: Observable[Boolean] = Observable(true)): Resource[F, JButton] = Resource.make(
    Sync[F].delay {
      val button = new JButton("foo")
      val labelCancel = label.doOnNext(newLabel => Task(button.setText(newLabel))).subscribe()
      //ignore Future[Ack]
      val eventToUnit: ActionListener = new ActionListener {
        al =>
        override def actionPerformed(e: ActionEvent): Unit = {
          clicks.onNext(())
            //unregister action listener
            .filter(_ == Ack.Stop).foreach(_ => button.removeActionListener(al))
        }
      }
      button.addActionListener(eventToUnit)
      val subsEnabled = enabled.doOnNext(b => Task {
        logger.debug("open button going " + b)
        button.setEnabled(b)
      }).subscribe()
      (button, Cancelable.collection(labelCancel, subsEnabled), eventToUnit)
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
