package docman

import javax.swing.table.AbstractTableModel
import scala.collection.mutable

/**
 * @author Thomas Geier
 * @since 10/26/13
 */
case class DocumentTableModel(_docs: IndexedSeq[Doc], properties: IndexedSeq[DocProperty]) extends AbstractTableModel{
  var docs = mutable.ArrayBuffer(_docs:_*)

  val updates = new mutable.HashMap[Int,Map[Int,String]]

  override def getColumnName(column: Int): String = column match {
    case 0 => "Edited"
    case 1 => "File"
    case x => properties(x - 2).name
  }

  def getRowCount: Int = docs.size
  def getColumnCount: Int = properties.size + 2
  def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = columnIndex match {
    case 0 => if(updates.isDefinedAt(rowIndex)) "+" else ""
    case 1 => docs(rowIndex).pdfFile.getName
    case c => updates.get(rowIndex).flatMap(_.get(columnIndex)).getOrElse(docs(rowIndex).properties(properties(c - 2)))
  }


  override def setValueAt(aValue: AnyRef, rowIndex: Int, columnIndex: Int) {
    updates.update(rowIndex, updates.getOrElse(rowIndex,Map()) + (columnIndex -> aValue.toString))
  }

  override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = columnIndex > 1

  def saveAllMeta() {
    updates.foreach{case (rowIdx, update) =>
      val doc = docs(rowIdx)
      val docUpdates = update.map{case (colIdx,value) =>
        properties(colIdx - 2) -> value
      }
      val newDoc = doc.copy(properties = doc.properties ++ docUpdates)
      Doc.saveMeta(newDoc)
      docs(rowIdx) = newDoc
    }
    updates.clear()
  }

  def getDocAtRow(row: Int) = docs(row)
}