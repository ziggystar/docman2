package docman

import java.io.{FilenameFilter, File}
import scala.swing._
import scala.swing.event.ValueChanged
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import javax.swing.{RowFilter, DefaultRowSorter, JLabel}
import migpanel.MigPanel
import javax.swing.RowFilter.Entry
import javax.imageio.ImageIO
import java.util.prefs.Preferences
import rx._
import rx.ops._
import scala.swing.FileChooser.SelectionMode

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

object Main extends Reactor {
  val preferences = Preferences.userNodeForPackage(this.getClass)


  val applicationTitle = "Docman2"
  val versionString = "1.0"

  val dbDirs: Var[Set[File]] = Var(
    preferences.get("db.dirs","").split(";").map(new File(_)).filter(_.exists()).toSet,
    name = "dbDirs"
  )

  val saveDBDie = dbDirs.foreach{ dirs =>
    preferences.put("db.dirs", dirs.mkString(";"))
  }

  val pdfs: Rx[IndexedSeq[File]] = Rx(Rx.Cookie, name = "pdfs"){
    val files = for{
      dir <- dbDirs()
      pdf <- dir.listFiles(new FilenameFilter { def accept(dir: File, name: String): Boolean = name.endsWith("pdf")})
    } yield pdf
    files.toIndexedSeq.sortBy(_.toString)
  }

  val docs: Rx[IndexedSeq[Doc]] = pdfs.map {
    files =>
      files map Doc.fromFile
  }

  val tableModel: DocumentTableModel = DocumentTableModel(docs(), DProp.ALL)
  val docUpdate = docs.foreach(tableModel.setDocs)

  class RxLabel(x: Rx[String]) extends Label(x.now){
    val updater = x.foreach(text = _)
  }

  def showPreferenceDialog(owner: Window): Unit = {
    val dia = new Dialog(owner)
    dia.title = s"$applicationTitle - Preferences"
    dia.minimumSize = new Dimension(600,400)
    val panel: MigPanel = new MigPanel()
    dia.contents = panel
    panel.add(new Label("DB Path"))
    panel.add(new RxLabel(dbDirs.map(_.map(_.toString).mkString(";"))),"wrap")
    panel.add(new Button(Action("Set"){
      val fc = new FileChooser()
      fc.fileSelectionMode = SelectionMode.DirectoriesOnly
      if (fc.showOpenDialog(panel) == FileChooser.Result.Approve)
        dbDirs.update(Set(fc.selectedFile))
    }))
    dia.pack()
    dia.resizable = false
    dia.open()
  }

  def main(args: Array[String]) {

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

    val leftPane = new MigPanel("fill","","[10]10[fill]")
    leftPane.add(Component.wrap(new JLabel("Filter")),"")
    leftPane.add(quickSearchBar,"wrap")
    val scrollPane: ScrollPane = new ScrollPane(Component.wrap(table))
    leftPane.add(scrollPane,"growx, pushy, span 2")

    val frame = new MainFrame{
      val icon = ImageIO.read(this.getClass.getClassLoader.getResourceAsStream("images/app_icon.png"))
      iconImage = icon
      title = f"Docman - $versionString"

      minimumSize = new Dimension(800,600)
      private val splitPane: SplitPane = new SplitPane(Orientation.Vertical, leftPane, viewer)
      splitPane.resizeWeight = 0.6
      contents = splitPane
    }


    val menuB = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(Action("Save All Meta"){tableModel.saveAllMeta()})
        contents += new MenuItem(Action("Open Selected PDF"){
          table.getSelectedDocuments.headOption.foreach(selectedPDF =>
            sys.process.Process(f"gnome-open ${selectedPDF.pdfFile.getAbsoluteFile}").run())
        })
        contents += new MenuItem(Action("Preferences"){showPreferenceDialog(frame)})
        contents += new MenuItem(Action("Exit"){frame.closeOperation()})
      }
    }

    frame.menuBar = menuB

    frame.open()
  }
}