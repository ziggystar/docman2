package docman.gui.table

import javax.swing.table.AbstractTableModel

import docman.core.{DProp, Doc, PropertyMap}

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

  val updates: mutable.Map[Int, PropertyMap] = new mutable.HashMap[Int, PropertyMap] with mutable.SynchronizedMap[Int, PropertyMap]
    .withDefaultValue(PropertyMap.empty)

  override def getColumnName(column: Int): String =  properties(column).name

  def getRowCount: Int = docs.size
  def getColumnCount: Int = properties.size
  def getValueAt(rowIndex: Int, columnIndex: Int): AnyRef = {
      val prop = propertyAtColumn(columnIndex).get
      updates(rowIndex).get(prop).orElse(docs(rowIndex).properties.get(prop)).orNull
  }

  def propertyAtColumn(c: Int): Option[DProp] = Some(c).map(c => properties(c))

  override def setValueAt(aValue: AnyRef, rowIndex: Int, columnIndex: Int) {
    val prop: DProp = propertyAtColumn(columnIndex).get
    updates.update(rowIndex, updates(rowIndex).put(prop)(aValue.asInstanceOf[prop.T]))
  }

  override def isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = true

  def saveAllMeta() {
    updates.foreach{case (rowIdx, updatedProperties) =>
      val doc = docs(rowIdx)
      val updatedDoc = doc.copy(properties = doc.properties ++ updatedProperties)
      docs(rowIdx) = updatedDoc
      updatedDoc.saveMeta()
    }
    updates.clear()
  }

  def getDocAtRow(row: Int): Doc = docs(row)
}