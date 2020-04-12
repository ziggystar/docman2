package docman.frontend.swing2.components

import java.awt.Component

import cats.effect.{Resource, Sync}
import com.typesafe.scalalogging.StrictLogging
import javax.swing.{JScrollPane, JTable, ListSelectionModel}
import javax.swing.event.ListSelectionEvent
import javax.swing.table.{AbstractTableModel, TableColumn}
import monix.eval.Task
import monix.execution.Cancelable
import monix.execution.Scheduler.Implicits.global
import monix.reactive._

import scala.collection.mutable.ArrayBuffer

object rxtable extends StrictLogging {

  case class Column[R](name: String, getValue: R => AnyRef = (_: R).toString){
    def tableColumn(i: Int): TableColumn = {
      val c = new TableColumn(i)
      c.setHeaderValue(name)
      c
    }
  }

  class MyTableModel[Id,R](columns: IndexedSeq[Column[R]]) extends AbstractTableModel {
    val data: ArrayBuffer[(Id, R)] = ArrayBuffer.empty
    override def getRowCount: Int = data.size
    override def getColumnCount: Int = columns.size
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = columns(columnIndex).getValue(data(rowIndex)._2)
    override def getColumnName(column: Int): String = columns(column).name
    override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = false
  }

  def rxtablemodel[F[_]: Sync, Id, R](columns: IndexedSeq[Column[R]],
                                      rows: Observable[(Id,Option[R])],
                                      updates: Observer[(Id,Option[R])]): Resource[F,MyTableModel[Id,R]] =
    for {
      tm <- Resource.pure(new MyTableModel[Id,R](columns))
      _ <- Resource.make[F,Cancelable](Sync[F].delay(rows.doOnNext(r => Task{
        r match {
          case (id, Some(r)) => tm.data.append((id,r))
          case (id, None) => tm.data.filterNot(_._1 == id)
        }
      }).subscribe()))(c => Sync[F].delay(c.cancel()))
    } yield tm

  /** Creates a table that displays and edits rows of type `R`. It does report updates to single rows and the current
    * selection.
    *
    * @param columns Description of columns.
    * @param rows The rows to display.
    * @param selection The currently selected rows.
    * @param updates Editor updates on rows.
    * @tparam Id Row identifier type.
    * @tparam R Row data type
    * @return Swing table.
    */
  def apply[F[_]: Sync, Id,R]( columns: IndexedSeq[Column[R]],
                               rows: Observable[(Id,Option[R])],
                               selection: Observer[Id] = Observer.empty,
                               updates: Observer[(Id,Option[R])] = Observer.empty
                             ): Resource[F,Component] = for {
    tm <- rxtablemodel(columns, rows, updates)
    table = {
      val t = new JTable(tm)
      t.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
      t.setAutoCreateRowSorter(true)
      t.getSelectionModel.addListSelectionListener((e: ListSelectionEvent) => {
        if (!e.getValueIsAdjusting)
          selection.onNext(tm.data(t.getSelectedRow)._1)
      })
      t
    }
  } yield new JScrollPane(table)
}
