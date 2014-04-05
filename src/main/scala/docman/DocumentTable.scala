package docman

import javax.swing.{JTable, ListSelectionModel}
import javax.swing.table.TableModel

/**
 * Swing table widget that displays a list of documents.
 * Allows sorting by columns.
 */
class DocumentTable(val documentModel: DocumentTableModel) extends JTable(documentModel) {
  //only select one document at a time
  getSelectionModel.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)

  //be able to sort by columns
  setAutoCreateRowSorter(true)

  adjustColumns()

  def adjustColumns(): Unit = {
    //set editor and renderer for each column
    for{
      col <- 0 until getColumnCount
      prop <- documentModel.propertyAtColumn(col).map(_.asInstanceOf[SwingTableProperty])
      tableColumn = getColumnModel.getColumn(col)
    } {
      tableColumn.setCellEditor(prop.cellEditor)
      tableColumn.setCellRenderer(prop.cellRenderer)
    }
  }


  override def setModel(dataModel: TableModel): Unit = {
    super.setModel(dataModel)
    adjustColumns()
  }

  def getSelectedDocuments: Iterable[Doc] = getSelectedRows.map(convertRowIndexToModel).map(documentModel.getDocAtRow)
}
