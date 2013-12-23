package docman

import scala.swing.Table
import javax.swing.ListSelectionModel

/**
 * Swing table widget that displays a list of documents.
 * Allows sorting by columns.
 */
class DocumentTable(val documentModel: DocumentTableModel) extends Table with SortableTable {
  model = documentModel

  //only select one document at a time
  peer.getSelectionModel.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)

  //be able to sort by columns
  peer.setAutoCreateRowSorter(true)

  //set editor and renderer for each column
  for{
    col <- 0 until peer.getColumnCount
    prop <- documentModel.propertyAtColumn(col).map(_.asInstanceOf[SwingTableProperty])
    tableColumn = peer.getColumnModel.getColumn(col)
  } {
    tableColumn.setCellEditor(prop.cellEditor)
    tableColumn.setCellRenderer(prop.cellRenderer)
  }

  def getSelectedDocuments: Iterable[Doc] = this.selection.rows.map(this.viewToModelRow).map(documentModel.getDocAtRow)
}
