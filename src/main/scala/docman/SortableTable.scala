package docman

import scala.swing.Table

/**
 * Implements the fix to make scala.swing.Table sortable:
 * @see https://issues.scala-lang.org/secure/attachment/13830/fixed_table_sorting.patch
 *
 * @author Thomas Geier
 * @since 12/15/13
 */
trait SortableTable { self: Table =>
  override def apply(row: Int, column: Int): Any = model.getValueAt(viewToModelRow(row), viewToModelColumn(column))

  def viewToModelRow(idx: Int) = peer.convertRowIndexToModel(idx)
  def modelToViewRow(idx: Int) = peer.convertRowIndexToView(idx)
  override def viewToModelColumn(idx: Int) = peer.convertColumnIndexToModel(idx)
  override def modelToViewColumn(idx: Int) = peer.convertColumnIndexToView(idx)
}