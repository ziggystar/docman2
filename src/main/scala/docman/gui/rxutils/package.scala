package docman.gui

import javax.swing.event.{DocumentEvent, DocumentListener}
import javax.swing.{Action => _, _}
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observable, Subject}

import scala.swing.{Action, Button, Label, TextField}

package object rxutils {

  def button(label: String): RControl[Unit] = {
    val obs = Subject[Unit]()
    val b = new Button(label){
      action = new Action(s"$label"){
        override def apply(): Unit = obs.onNext(())
      }
    }
    RControl(b,obs)
  }

  def label(x: Observable[String]): Label = new Label{
    x.foreach(this.text = _)
  }

  def textField(initial: String = "", columns: Int = -1): RControl[String] = {
    val tf = if(columns < 1) new TextField(initial) else new TextField(initial, columns)
    val s = BehaviorSubject[String](initial)
    tf.peer.getDocument.addDocumentListener(new DocumentListener {
      override def removeUpdate(e: DocumentEvent): Unit = s.onNext(tf.text)
      override def changedUpdate(e: DocumentEvent): Unit = s.onNext(tf.text)
      override def insertUpdate(e: DocumentEvent): Unit = s.onNext(tf.text)
    })
    RControl(tf,s)
  }
}
