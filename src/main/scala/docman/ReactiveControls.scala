package docman

import java.awt.event.ActionEvent
import java.io.File

import docman.components.MigPanel
import javax.swing.event.TableModelEvent
import javax.swing.table.DefaultTableModel
import javax.swing.{Action => _, _}
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observable, Subject}

import scala.collection.JavaConverters._
import scala.swing.FileChooser.SelectionMode
import scala.swing.{Action, Button, Component, FileChooser, TextField}

object ReactiveControls {

  case class RControl[T](component: Component, obs: Observable[T])

  def button(label: String): RControl[Unit] = {
    val obs = Subject[Unit]()
    val b = new Button(label){
      action = new Action(s"$label"){
        override def apply(): Unit = obs.onNext(())
      }
    }
    RControl(b,obs)
  }

  def textField(text: String = "", columns: Int = -1): RControl[String] = {
    val tf = if(columns < 1) new TextField(text) else new TextField(text, columns)
    val s = Subject[String]
    tf.peer.addActionListener((_: ActionEvent) => s.onNext(tf.text))
    RControl(tf,s)
  }

  object dialogs {
    trait Dialog[T]{
      def build(initial: T): RControl[T]
    }

    val FileList: Dialog[Seq[(File, Boolean)]] = initial => {
      val dia = new MigPanel()
      val tm = new DefaultTableModel(
        new java.util.Vector(Seq("Directory", "recursive").asJavaCollection),
        0
      ){
        override def isCellEditable(row: Int, column: Int): Boolean = false
      }

      initial.foreach{ row =>
        tm.addRow(Array[AnyRef](row._1,java.lang.Boolean.valueOf(row._2)))
      }
      val table = new JTable(tm)


      val buttonAdd = button("Add directory")
      buttonAdd.obs.subscribe{ _ =>
        val fc = new FileChooser()
        fc.fileSelectionMode = SelectionMode.DirectoriesOnly
        if (fc.showOpenDialog(dia) == FileChooser.Result.Approve)
          tm.addRow(Array[AnyRef](fc.selectedFile,java.lang.Boolean.FALSE))
      }
      val buttonRemove = button("Remove selected")
      buttonRemove.obs.subscribe{_ =>
        table.getSelectedRows.sorted.reverse.foreach(tm.removeRow)
      }
      val buttonToggleRec = button("Toggle recursive")
      buttonToggleRec.obs.subscribe{ _ =>
        table.getSelectedRows
          .foreach{ row =>
            tm.setValueAt(!tm.getValueAt(row,1).asInstanceOf[Boolean],row,1)
        }
      }

      dia.add(Component.wrap(new JScrollPane(table)),"span 1 3")
      dia.add(buttonToggleRec.component, "growx, wrap")
      dia.add(buttonAdd.component, "growx, wrap")
      dia.add(buttonRemove.component, "growx, top, wrap")

      val values = BehaviorSubject[Seq[(File,Boolean)]](initial)
      tm.addTableModelListener((_: TableModelEvent) => {
        val seq = (0 until tm.getRowCount).map(r =>
          (tm.getValueAt(r, 0).asInstanceOf[File], tm.getValueAt(r, 1).asInstanceOf[Boolean])
        )
        values.onNext(seq)
      })

      RControl(dia,values)
    }
  }
}
