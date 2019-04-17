package docman.gui.dialogs

import java.io.File

import com.typesafe.scalalogging.StrictLogging
import docman.config.{Config, SearchDir}
import docman.gui.MigPanel
import docman.gui.rxutils._
import javax.swing.event.TableModelEvent
import javax.swing.table.DefaultTableModel
import javax.swing.{JScrollPane, JTable}
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observable, Subject}

import scala.collection.JavaConverters._
import scala.swing.FileChooser.SelectionMode
import scala.swing._

case object PreferenceDialog extends RXDialog[Config] with StrictLogging {

  override def build(initial: Config): RControl[Config] = {
    val dia = new MigPanel()
    val tm = new DefaultTableModel(
      new java.util.Vector(Seq("Directory", "recursive").asJavaCollection),
      0
    ) {
      override def isCellEditable(row: Int, column: Int): Boolean = false
    }

    initial.searchDirs.foreach { row =>
      tm.addRow(Array[AnyRef](row.dir, java.lang.Boolean.valueOf(row.recursive)))
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

    val values = BehaviorSubject[Config](initial)
    tm.addTableModelListener((_: TableModelEvent) => {
      val seq = (0 until tm.getRowCount).map(r =>
        SearchDir(tm.getValueAt(r, 0).asInstanceOf[File], tm.getValueAt(r, 1).asInstanceOf[Boolean])
      )
      values.onNext(Config(seq))
    })
    RControl(dia, values)
  }

  def show(owner: Window, current: Config): Observable[Config] = {
    logger.info("opening prefs with " + current)
    val dia = new swing.Dialog(owner)
    dia.title = s"${owner.peer.getRootPane.getUIClassID} - Preferences"
    dia.minimumSize = new Dimension(600,400)
    val panel: MigPanel = new MigPanel()
    dia.contents = panel
    val dirList = PreferenceDialog.build(current)

    panel.add(new Label("Library Paths"), "wrap")
    panel.add(dirList.component, "wrap")
    val btnOk = button("Ok")
    val btnCancel = button("Cancel")
    panel.add(btnOk.component, "tag ok, split 2")
    panel.add(btnCancel.component, "tag cancel")

    val closeO = Subject[Unit]()
    val closeSubscription = (btnCancel.obs merge btnOk.obs)
      .subscribe{ _ => {
        dia.close()
        closeO.onNext(())
        closeO.onCompleted()
      }
      }
    dia.pack()
    dia.resizable = false
    dia.open()

    dirList.obs.takeUntil(btnOk.obs).last
  }
}