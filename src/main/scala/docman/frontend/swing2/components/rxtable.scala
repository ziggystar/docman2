package docman.frontend.swing2.components

import java.awt.Component

import cats.effect.{Resource, Sync}
import com.typesafe.scalalogging.StrictLogging
import javax.swing.event.ListSelectionEvent
import javax.swing.table.{AbstractTableModel, TableColumn}
import javax.swing.{JScrollPane, JTable, ListSelectionModel}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.{Ack, Cancelable}
import monix.reactive._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Future

object rxtable extends StrictLogging {

  case class Column[R](name: String, getValue: R => AnyRef = (_: R).toString, prefWidth: Option[Int] = None, isEditable: Boolean = false){
    def tableColumn(i: Int): TableColumn = {
      val c = new TableColumn(i)
      c.setHeaderValue(name)
      prefWidth.foreach(c.setPreferredWidth)
      c
    }
  }

  class MyTableModel[Id,R](columns: IndexedSeq[Column[R]], observer: Observer[(Id,Option[R])]) extends AbstractTableModel {
    private val data: ArrayBuffer[(Id, R)] = ArrayBuffer.empty
    def getRow(i: Int): (Id,R) = data(i)
    def setRow(r: (Id, R)): Future[Ack] = {
      data.view.zipWithIndex.find(_._1._1 == r._1) match {
        case None =>
          data.insert(0,r)
          fireTableRowsInserted(0, 0)
        case Some((_,i)) =>
          data.update(i, r)
          fireTableRowsUpdated(i,i)
      }
      observer.onNext((r._1, Some(r._2)))
    }
    def removeRow(rid: Id): Future[Ack] = {
      data.view.zipWithIndex.find(_._1._1 == rid).foreach{x =>
        data.remove(x._2)
        fireTableRowsDeleted(x._2, x._2)
      }
      observer.onNext((rid, None))
    }

    override def getRowCount: Int = data.size
    override def getColumnCount: Int = columns.size
    override def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = columns(columnIndex).getValue(data(rowIndex)._2)
    override def getColumnName(column: Int): String = columns(column).name
    override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = columns(columnIndex).isEditable
  }

  def rxtablemodel[F[_]: Sync, Id, R](columns: IndexedSeq[Column[R]],
                                      rows: Observable[(Id,Option[R])],
                                      updates: Observer[(Id,Option[R])]): Resource[F,MyTableModel[Id,R]] =
    for {
      tm <- Resource.pure(new MyTableModel[Id,R](columns, updates))
      _ <- Resource.make[F,Cancelable](Sync[F].delay(rows.doOnNext(r => Task.fromFuture(
        r match {
          case (id, Some(r)) => tm.setRow((id, r))
          case (id, None) => tm.removeRow(id)
        }).map(_ => ())).subscribe()))(c => Sync[F].delay(c.cancel()))
    } yield tm

  /** Creates a table that displays and edits rows of type `R`. It does report updates to single rows and the current
    * selection.
    *
    * @param columns Description of columns.
    * @param rows The rows to display.
    * @param selection The currently selected rows.
    * @param updates Editor updates on rows.
    * @tparam RId Row identifier type.
    * @tparam R Row data type
    * @return Swing table.
    */
  def apply[F[_]: Sync, RId,R](columns: IndexedSeq[Column[R]],
                               rows: Observable[(RId,Option[R])],
                               selection: Observer[RId] = Observer.empty,
                               updates: Observer[(RId,Option[R])] = Observer.empty
                             ): Resource[F,Component] = for {
    tm <- rxtablemodel(columns, rows, updates)
    table = {
      val t = new JTable(tm)
      t.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
      t.setAutoCreateRowSorter(true)
      t.getSelectionModel.addListSelectionListener((e: ListSelectionEvent) => {
        if (!e.getValueIsAdjusting)
          selection.onNext(tm.getRow(t.getSelectedRow)._1)
      })
      t
    }
  } yield new JScrollPane(table)
}
