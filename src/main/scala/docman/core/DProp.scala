package docman.core

import java.awt.Component
import java.io.{File, FileWriter}
import java.sql.Date
import java.text.DateFormat

import cats.effect.{IO, Resource}
import javax.swing.table.DefaultTableCellRenderer
import javax.swing.{DefaultCellEditor, JTable, JTextField}
import org.apache.pdfbox.pdmodel.PDDocumentInformation

import scala.io.Source
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
  val serPrefix = f"${name.toUpperCase}:"

  final def deserialize(s: String): Option[this.T] =
    Some(s).filter(_.startsWith(serPrefix)).map(_.drop(serPrefix.length)).flatMap(mydeserialize)
  final def serialize(x: T): String = {
    val string: String = myserialize(x)
    assert(!string.contains("\n"), "serialized string must not contain newline")
    f"$serPrefix$string"
  }

  protected def mydeserialize(s: String): Option[this.T]
  protected def myserialize(t: T): String
}

trait PDFExtractor{ self: DProp =>
  def pdfboxExtractor: PDDocumentInformation => Option[T]
}

trait StringStringSerializable extends LineSerializer { self: DProp{type T = String} =>
  def mydeserialize(s: String): Option[String] = Some(s)
  def myserialize(t: String): String = t
}

trait SwingTableProperty{
  def cellRenderer: DefaultTableCellRenderer = new DefaultTableCellRenderer
  def cellEditor: Option[DefaultCellEditor] = Option(new DefaultCellEditor(new JTextField))
}

object SwingTableProperty{
  def stringRenderer[T](_f: T => String): DefaultTableCellRenderer {
    def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component

    def setValue(value: Any): Unit

    val f: T => String
  } = new DefaultTableCellRenderer{
    val f: T => String = (t: T) => Option(t).map(_f).orNull
    override def getTableCellRendererComponent(table: JTable,
                                               value: scala.Any,
                                               isSelected: Boolean,
                                               hasFocus: Boolean,
                                               row: Int,
                                               column: Int): Component =
      super.getTableCellRendererComponent(table,value,isSelected,hasFocus,row,column)

    override def setValue(value: scala.Any): Unit = super.setValue(f(value.asInstanceOf[T]))
  }
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

  def displayFormat: DateFormat = DateFormat.getDateInstance(DateFormat.MEDIUM, java.util.Locale.forLanguageTag("de"))
  def shortFormat: DateFormat = DateFormat.getDateInstance(DateFormat.SHORT, java.util.Locale.forLanguageTag("de"))

  /** The name of the property. */
  def name: String = "Date"

  def mydeserialize(s: String): Option[Date] = Try(Date.valueOf(s)).toOption
  def myserialize(x: Date): String = x.toString

  override def cellRenderer: DefaultTableCellRenderer = SwingTableProperty.stringRenderer[Date](d => displayFormat.format(new java.util.Date(d.getTime)))

  override def cellEditor: Option[DefaultCellEditor] = Option(new DefaultCellEditor(new JTextField){
    override def getCellEditorValue: AnyRef = Try(Date.valueOf(getComponent.asInstanceOf[JTextField].getText))
      .recoverWith[Date]{ case _ => Try(new Date(shortFormat.parse(getComponent.asInstanceOf[JTextField].getText).getTime))}
      .getOrElse(null)
  })
}

object ModificationDateDP extends DProp with LineSerializer with SwingTableProperty{
  type T = Date

  def displayFormat: DateFormat = DateFormat.getDateInstance(DateFormat.MEDIUM, java.util.Locale.forLanguageTag("de"))
  def shortFormat: DateFormat = DateFormat.getDateInstance(DateFormat.SHORT, java.util.Locale.forLanguageTag("de"))

  /** The name of the property. */
  def name: String = "Modified"

  def mydeserialize(s: String): Option[Date] = Try(Date.valueOf(s)).toOption
  def myserialize(x: Date): String = x.toString

  override def cellRenderer: DefaultTableCellRenderer = SwingTableProperty.stringRenderer[Date](d => displayFormat.format(new java.util.Date(d.getTime)))

  override def cellEditor: Option[DefaultCellEditor] = None
}

/** The tags may not contain newlines or commas. */
object TagListDP extends DProp with LineSerializer with SwingTableProperty {
  override type T = Set[String]
  /** The name of the property. */
  override def name: String = "Tags"
  override protected def myserialize(t: TagListDP.T): String = t.mkString(",")
  override protected def mydeserialize(s: String): Option[TagListDP.T] =
    Some(s.split(',').map(_.trim).filterNot(_.isEmpty).toSet).filterNot(_.isEmpty)

  override def cellRenderer: DefaultTableCellRenderer = SwingTableProperty.stringRenderer[Set[String]](d => d.mkString(","))

  override def cellEditor: Option[DefaultCellEditor] = Option(
    new DefaultCellEditor(new JTextField){
      override def getCellEditorValue: AnyRef = mydeserialize(getComponent.asInstanceOf[JTextField].getText).orNull

    override def getTableCellEditorComponent(table: JTable, value: scala.Any, isSelected: Boolean, row: Int, column: Int): Component = {
      val c = super.getTableCellEditorComponent(table, value, isSelected, row, column).asInstanceOf[JTextField]
      Option(value).foreach(tags => c.setText(tags.asInstanceOf[T].mkString(",")))
      c
    }
  }
  )
}

case class PropertyMap protected(m: Map[DProp,AnyRef]){
  def get(p: DProp): Option[p.T] = m.get(p).map(_.asInstanceOf[p.T])
  def put[P <: DProp](p: P)(v: p.T): PropertyMap = new PropertyMap(m + (p->v))
  def ++(pm: PropertyMap): PropertyMap = new PropertyMap(m ++ pm.m)
  def serializeToLines: Iterable[String] = m.collect{
    case (k,v) if v != null =>
      val prop = k.asInstanceOf[DProp with LineSerializer]
      prop.serialize(v.asInstanceOf[prop.T])
  }
}

object PropertyMap{
  def empty: PropertyMap = new PropertyMap(Map())
}

object DProp {
  val ALL: IndexedSeq[DProp with LineSerializer with SwingTableProperty] = IndexedSeq(AuthorDP, SubjectDP, DateDP, TagListDP, ModificationDateDP)

  def readPropertiesFromPDF(pd: PDDocumentInformation): PropertyMap = {
    val props = for {
      prop <- ALL.collect{case pdfProp: DProp with PDFExtractor => pdfProp}
      value <- prop.pdfboxExtractor(pd)
    } yield PropertyMap.empty.put(prop)(value)
    props.foldLeft(PropertyMap.empty)(_ ++ _)
  }

  /**
    * @param f A file containing line-serialized properties.
    * @return Property map read from the file. */
  def readPropertiesFromFile(f: File): PropertyMap =
    Resource.fromAutoCloseable(IO(Source.fromFile(f)))
      .use(bs => {
        IO {
          val lines = bs.getLines()
          val props = for {
            line <- lines
            prop <- ALL
            value <- prop.deserialize(line)
          } yield PropertyMap.empty.put(prop)(value)
          props.foldLeft(PropertyMap.empty)(_ ++ _)
        }
      })
    .unsafeRunSync()

  def writePropertiesToFile(props: PropertyMap, f: File): Unit =
    Resource.fromAutoCloseable(IO(new FileWriter(f)))
    .use(fos => IO(fos.write(props.serializeToLines.mkString("\n"))))
    .unsafeRunSync()
}
