package docman

import javax.swing.table.DefaultTableCellRenderer
import javax.swing.{JTextField, DefaultCellEditor}
import java.sql.Date
import scala.util.Try

/**
 * Typed document property class.
 */


trait DProp{
  type T <: AnyRef
  /** The name of the property. */
  def name: String
}

trait LineSerializer{ self: DProp =>
  val serPrefix = f"$name:"
  final def deserialize(s: String): Option[T] =
    Some(s).filter(_.startsWith(serPrefix)).map(_.drop(serPrefix.size)).flatMap(mydeserialize)
  final def serialize(x: T): String = f"$serPrefix${myserialize(x)}"

  def mydeserialize(s: String): Option[T]
  def myserialize(t: T): String
}

trait StringStringSerializable extends LineSerializer { self: DProp{type T = String} =>
  def mydeserialize(s: String): Option[String] = Some(s)
  def myserialize(t: String): String = t
}

trait SwingTableProperty{
  def cellRenderer: DefaultTableCellRenderer = new DefaultTableCellRenderer
  def cellEditor: DefaultCellEditor = new DefaultCellEditor(new JTextField)
}

trait StringProp extends DProp with StringStringSerializable with SwingTableProperty{
  type T = String
}

object AuthorDP extends StringProp {
  def name: String = "Author"
}
object SubjectDP extends StringProp{
  def name: String = "Subject"
}
object DateDP extends DProp with LineSerializer with SwingTableProperty{
  type T = Date

  /** The name of the property. */
  def name: String = "Date"

  def mydeserialize(s: String): Option[Date] = Try(Date.valueOf(s)).toOption
  def myserialize(x: Date): String = x.toString
}

object DProp {
  val ALL = IndexedSeq(AuthorDP, SubjectDP, DateDP)
}
