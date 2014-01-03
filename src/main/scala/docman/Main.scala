package docman

import java.io.{FilenameFilter, File}
import scala.swing._
import scala.swing.event.{ValueChanged, TableRowsSelected}
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import javax.swing.{RowFilter, DefaultRowSorter, JLabel, JTextField}
import migpanel.MigPanel
import javax.swing.RowFilter.Entry

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

object Main extends Reactor {
  val dir = new File("documents/A")
  val pdfs: Array[File] =
    dir.listFiles(new FilenameFilter { def accept(dir: File, name: String): Boolean = name.endsWith("pdf")})

  def main(args: Array[String]) {
    val docs = pdfs.map(Doc.fromFile)
    val tableModel = DocumentTableModel(docs, DProp.ALL)

    val table = new DocumentTable(tableModel)

    val viewer = new PDFViewer

    table.getSelectionModel.addListSelectionListener(new ListSelectionListener {
      def valueChanged(e: ListSelectionEvent): Unit = {
        if(!e.getValueIsAdjusting)
          table.getSelectedDocuments.headOption.foreach(d => viewer.setFile(Some(d.pdfFile)))
      }
    })

    val quickSearchBar = new TextField(30)
    this.listenTo(quickSearchBar)
    reactions += {
      case ValueChanged(_) => table.getRowSorter.asInstanceOf[DefaultRowSorter[DocumentTableModel,Int]].setRowFilter(
        new RowFilter[DocumentTableModel,Int]{
          def include(entry: Entry[_ <: DocumentTableModel, _ <: Int]): Boolean = 
            Seq(2,3).exists(c => entry.getStringValue(c).toLowerCase.contains(quickSearchBar.text.toLowerCase))
        }
      )
    }

    val menuB = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Save All Meta"){tableModel.saveAllMeta()})
        contents += new MenuItem(Action("Open Selected PDF"){
          table.getSelectedDocuments.headOption.foreach(selectedPDF =>
            sys.process.Process(f"gnome-open ${selectedPDF.pdfFile.getAbsoluteFile}").run())

        })
      }
    }

    val leftPane = new MigPanel
    leftPane.add(Component.wrap(new JLabel("Filter")),"")
    leftPane.add(quickSearchBar,"wrap")
    leftPane.add(new ScrollPane(Component.wrap(table)),"span 2")

    val frame = new MainFrame{
      menuBar = menuB
      minimumSize = new Dimension(800,600)
      contents = new SplitPane(Orientation.Vertical,leftPane,viewer)
    }

    frame.open()
  }
}
