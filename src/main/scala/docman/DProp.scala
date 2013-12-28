package docman

import javax.swing.table.DefaultTableCellRenderer
import javax.swing.{JTable, JTextField, DefaultCellEditor}
import java.sql.Date
import scala.util.Try
import java.io.{FileWriter, File}
import scala.io.Source
import org.apache.pdfbox.pdmodel.PDDocumentInformation
import java.awt.Component

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
    Some(s).filter(_.startsWith(serPrefix)).map(_.drop(serPrefix.size)).flatMap(mydeserialize)
  final def serialize(x: T): String = f"$serPrefix${myserialize(x)}"

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
  def cellEditor: DefaultCellEditor = new DefaultCellEditor(new JTextField)
}

object SwingTableProperty{
  def stringRenderer[T](f: T => String) = new DefaultTableCellRenderer{
    override def getTableCellRendererComponent(table: JTable,
                                               value: scala.Any,
                                               isSelected: Boolean,
                                               hasFocus: Boolean,
                                               row: Int,
                                               column: Int): Component = {
      super.getTableCellRendererComponent(table,f(value.asInstanceOf[T]),isSelected,hasFocus,row,column)
    }

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

  /** The name of the property. */
  def name: String = "Date"

  def mydeserialize(s: String): Option[Date] = Try(Date.valueOf(s)).toOption
  def myserialize(x: Date): String = x.toString

  override def cellRenderer: DefaultTableCellRenderer = SwingTableProperty.stringRenderer((_: AnyRef) => "hi")
}

case class PropertyMap protected(m: Map[DProp,AnyRef]){
  def get(p: DProp): Option[p.T] = m.get(p).map(_.asInstanceOf[p.T])
  def put[P <: DProp](p: P)(v: p.T): PropertyMap = new PropertyMap(m + (p->v))
  def ++(pm: PropertyMap): PropertyMap = new PropertyMap(m ++ pm.m)
  def serializeToLines: Iterable[String] = m.map{
    case (k,v) => {
      val prop = k.asInstanceOf[DProp with LineSerializer]
      prop.serialize(v.asInstanceOf[prop.T])
    }
  }
}

object PropertyMap{
  def empty: PropertyMap = new PropertyMap(Map())
  def apply[DP <: DProp,P <: DP](kv: (P,P#T)): PropertyMap = new PropertyMap(Map(kv))
}

object DProp {
  import resource._
  val ALL: IndexedSeq[DProp with LineSerializer with SwingTableProperty] = IndexedSeq(AuthorDP, SubjectDP, DateDP)

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
  def readPropertiesFromFile(f: File): PropertyMap = {
    val lines = managed(Source.fromFile(f)).map(_.getLines().toArray).opt.getOrElse(Array())
    val props = for {
      line <- lines
      prop <- ALL
      value <- prop.deserialize(line)
    } yield PropertyMap.empty.put(prop)(value)
    props.foldLeft(PropertyMap.empty)(_ ++ _)
  }

  def writePropertiesToFile(props: PropertyMap, f: File): Unit = {
    for( fos <- managed(new FileWriter(f))){
      fos.write(props.serializeToLines.mkString("\n"))
    }
  }
}
