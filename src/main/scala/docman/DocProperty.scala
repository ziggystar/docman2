package docman

import org.apache.pdfbox.pdmodel.PDDocumentInformation
import javax.swing.table.DefaultTableCellRenderer
import java.text.SimpleDateFormat
import javax.swing.{JTextField, DefaultCellEditor}

/**
 * @author Thomas Geier
 * @since 10/26/13
 */
trait DocProperty {
  def cellRenderer = new DefaultTableCellRenderer
  def cellEditor = new DefaultCellEditor(new JTextField)
  def isEditable: Boolean = true
  def name: String
  def defaultValue: String
  def pdfboxExtractor: PDDocumentInformation => String
  def serialPrefix: String
  def serializeToLine(a: String): String = f"$serialPrefix:$a"
  def deserialize(line: String): Option[String] =
    Option(line).filter(_.startsWith(f"$serialPrefix:")).map(_.drop(serialPrefix.size + 1))
}



object DocProperty {
  val ALL = IndexedSeq(AuthorProp, SubjectProp, DateProp)
}

object AuthorProp extends DocProperty{
  def name: String = "Author"
  def defaultValue: String = ""
  def pdfboxExtractor: (PDDocumentInformation) => String =  di => di.getAuthor
  def serialPrefix: String = "AUTHOR"
}

object SubjectProp extends DocProperty{
  def name: String = "Subject"
  def defaultValue: String = ""
  def pdfboxExtractor: (PDDocumentInformation) => String =  di => di.getSubject
  def serialPrefix: String = "SUBJECT"
}

object DateProp extends DocProperty{
  override def cellRenderer = new DefaultTableCellRenderer {
    val f = new SimpleDateFormat("MM/dd/yy")

    override def setValue(value: scala.Any) {
      setText(if(value == null) "" else f.format(value))
    }
  }

  def name: String = "Date"

  def defaultValue: String = "1.1.2013"

  def pdfboxExtractor: (PDDocumentInformation) => String = di => ""

  def serialPrefix: String = "Date"
}