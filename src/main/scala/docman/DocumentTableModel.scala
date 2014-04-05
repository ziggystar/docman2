package docman

import javax.swing.table.AbstractTableModel
import scala.collection.mutable

/**
 * @author Thomas Geier
 * @since 10/26/13
 */
case class DocumentTableModel(_docs: IndexedSeq[Doc], properties: IndexedSeq[DProp]) extends AbstractTableModel{
  private var docs = mutable.ArrayBuffer(_docs:_*)

  def setDocs(newDocs: Seq[Doc]): Unit = {
    docs.clear()
    docs ++= newDocs
    fireTableDataChanged()
  }

  val updates = new mutable.HashMap[Int, PropertyMap] with mutable.SynchronizedMap[Int, PropertyMap]
    .withDefaultValue(PropertyMap.empty)

  override def getColumnName(column: Int): String = column match {
    case 0 => "Edited"
    case 1 => "File"
    case x => properties(x - 2).name
  }

  def getRowCount: Int = docs.size
  def getColumnCount: Int = properties.size + 2
  def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = columnIndex match {
    case 0 =>
      if(updates.isDefinedAt(rowIndex)) Boolean.box(true) else Boolean.box(false)
    case 1 =>
      docs(rowIndex).pdfFile.getName
    case c =>
      val prop = propertyAtColumn(c).get
      updates(rowIndex).get(prop).orElse(docs(rowIndex).properties.get(prop)).orNull
  }

  def propertyAtColumn(c: Int): Option[DProp] = Some(c).filter(_ >= 2).map(c => properties(c-2))


  override def setValueAt(aValue: AnyRef, rowIndex: Int, columnIndex: Int) {
    val prop: DProp = propertyAtColumn(columnIndex).get
    updates.update(rowIndex, updates(rowIndex).put(prop)(aValue.asInstanceOf[prop.T]))
  }

  override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = columnIndex >= 2

  def saveAllMeta() {
    updates.foreach{case (rowIdx, updatedProperties) =>
      val doc = docs(rowIdx)
      val updatedDoc = doc.copy(properties = doc.properties ++ updatedProperties)
      docs(rowIdx) = updatedDoc
      updatedDoc.saveMeta()
    }
    updates.clear()
  }

  def getDocAtRow(row: Int) = docs(row)
}