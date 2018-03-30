package docman.gui.dialogs

import java.io.File

import docman.gui.MigPanel
import docman.gui.rxutils._
import javax.swing.event.TableModelEvent
import javax.swing.{JScrollPane, JTable}
import javax.swing.table.DefaultTableModel
import rx.lang.scala.subjects.BehaviorSubject

import scala.collection.JavaConverters._
import scala.swing._
import scala.swing.FileChooser.SelectionMode

case object PreferenceDialog extends RXDialog[Seq[(File, Boolean)]] {

  override def build(initial: Seq[(File, Boolean)]): RControl[Seq[(File, Boolean)]] = {
    val dia = new MigPanel()
    val tm = new DefaultTableModel(
      new java.util.Vector(Seq("Directory", "recursive").asJavaCollection),
      0
    ) {
      override def isCellEditable(row: Int, column: Int): Boolean = false
    }

    initial.foreach { row =>
      tm.addRow(Array[AnyRef](row._1, java.lang.Boolean.valueOf(row._2)))
    }
    val table = new JTable(tm)


    val buttonAdd = button("Add directory")
    buttonAdd.obs.subscribe { _ =>
      val fc = new FileChooser()
      fc.fileSelectionMode = SelectionMode.DirectoriesOnly
      if (fc.showOpenDialog(dia) == FileChooser.Result.Approve)
        tm.addRow(Array[AnyRef](fc.selectedFile, java.lang.Boolean.FALSE))
    }
    val buttonRemove = button("Remove selected")
    buttonRemove.obs.subscribe { _ =>
      table.getSelectedRows.sorted.reverse.foreach(tm.removeRow)
    }
    val buttonToggleRec = button("Toggle recursive")
    buttonToggleRec.obs.subscribe { _ =>
      table.getSelectedRows
        .foreach { row =>
          tm.setValueAt(!tm.getValueAt(row, 1).asInstanceOf[Boolean], row, 1)
        }
    }

    dia.add(Component.wrap(new JScrollPane(table)), "span 1 3")
    dia.add(buttonToggleRec.component, "growx, wrap")
    dia.add(buttonAdd.component, "growx, wrap")
    dia.add(buttonRemove.component, "growx, top, wrap")

    val values = BehaviorSubject[Seq[(File, Boolean)]](initial)
    tm.addTableModelListener((_: TableModelEvent) => {
      val seq = (0 until tm.getRowCount).map(r =>
        (tm.getValueAt(r, 0).asInstanceOf[File], tm.getValueAt(r, 1).asInstanceOf[Boolean])
      )
      values.onNext(seq)
    })

    RControl(dia, values)
  }
}
