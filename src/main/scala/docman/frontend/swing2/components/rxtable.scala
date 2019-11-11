package docman.frontend.swing2.components

import java.awt.Component

import cats.effect.{Resource, Sync}
import com.typesafe.scalalogging.StrictLogging
import javax.swing.table.{DefaultTableColumnModel, DefaultTableModel, TableCellRenderer, TableColumn}
import javax.swing.{JButton, JScrollPane, JTable}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.reactive._

object rxtable extends StrictLogging {

  case class Column[R](name: String, getValue: R => AnyRef = (_: R).toString){
    def tableColumn(i: Int): TableColumn = {
      val c = new TableColumn(i)
      c.setHeaderValue(name)
      c
    }
  }

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
                               rows: Observable[IndexedSeq[(Id,R)]],
                               selection: Observer[IndexedSeq[Id]] = Observer.empty,
                               updates: Observer[(Id,R)] = Observer.empty
                             ): Resource[F,Component] = {
    val tableModel: DefaultTableModel = new DefaultTableModel(0, columns.size)
    val columnModel: DefaultTableColumnModel = new DefaultTableColumnModel()
    val column = new TableColumn(1, 10)
    columns.zipWithIndex.map(ci => ci._1.tableColumn(ci._2)).foreach(columnModel.addColumn)
    val table = new JTable(tableModel, columnModel)

    Resource.make(
      Sync[F].delay(
        rows.doOnNext(xs => Task.eval {
          logger.info(s"updating table content with ${xs.size} rows")
          tableModel.setDataVector(
            xs.map(x => columns.map(_.getValue(x._2))(collection.breakOut): Array[AnyRef])(collection.breakOut): Array[Array[AnyRef]],
            columns.map(_.name)(collection.breakOut): Array[AnyRef]
          )
        }).subscribe)
    )(c =>
      Sync[F].delay(c.cancel())
    ).map(_ => new JScrollPane(table))
  }

}
