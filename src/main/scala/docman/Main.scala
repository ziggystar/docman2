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
import rxutils.preferences._
import scala.swing.FileChooser.SelectionMode
import java.util.Locale
import rxutils.swing.{RxComboBox, RxLabel}
import utils.StringIso
import java.text.DateFormat

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

case class AppMain(preferences: Preferences) extends Reactor {
  val applicationTitle = "Docman2"
  val versionString = "1.0"

  val dbDirs: Var[Set[File]] = Var(
    preferences.get("db.dirs","").split(";").map(new File(_)).filter(_.exists()).toSet,
    name = "dbDirs"
  )

  val saveDBDie: Obs = dbDirs.foreach{ dirs =>
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
  val docUpdate: Obs = docs.foreach(tableModel.setDocs)

  def showPreferenceDialog(owner: Window): Unit = {
    val dia = new Dialog(owner)
    dia.title = s"$applicationTitle - Preferences"
    dia.minimumSize = new Dimension(600,400)
    val panel: MigPanel = new MigPanel()
    dia.contents = panel
    panel.add(new Label("DB Path"))
    panel.add(new RxLabel(dbDirs.map(_.map(_.toString).mkString(";"))),"split 2")
    panel.add(new Button(Action("Set"){
      val fc = new FileChooser()
      fc.fileSelectionMode = SelectionMode.DirectoriesOnly
      if (fc.showOpenDialog(panel) == FileChooser.Result.Approve)
        dbDirs.update(Set(fc.selectedFile))
    }),"wrap")
    dia.pack()
    dia.resizable = false
    dia.open()
  }

  def showAboutDialog(owner: Window): Unit = {
    val dia = new Dialog(owner){
      title = s"$applicationTitle - About"
      minimumSize = new Dimension(200,200)
      val panel = new MigPanel()
      panel.add(new Label("Version"))
      panel.add(new Label(versionString),"wrap")
      contents = panel
    }
    dia.pack()
    dia.resizable = false
    dia.open()
  }

  val table: DocumentTable = new DocumentTable(tableModel)

  val selectedDocuments: Var[Set[Doc]] = Var(Set())

  table.getSelectionModel.addListSelectionListener(new ListSelectionListener {
    def valueChanged(e: ListSelectionEvent): Unit = {
      if(!e.getValueIsAdjusting)
        selectedDocuments.update(table.getSelectedDocuments.toSet)
    }
  })

  val displayedPdf = selectedDocuments.map{
    case selected if selected.size == 1 => Some(selected.head.pdfFile)
    case _ => None
  }

  val viewer = PDFViewer.newViewer(displayedPdf)

  val quickSearchBar: TextField = new TextField(30)
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

    override def closeOperation() {
      visible = true
      tableModel.saveAllMeta()
      super.closeOperation()
    }
  }


  val menuB = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(Action("Save All Meta"){tableModel.saveAllMeta()})
      contents += new MenuItem(Action("Open Selected PDF"){
        table.getSelectedDocuments.headOption.foreach(selectedPDF =>
          sys.process.Process(f"gnome-open ${selectedPDF.pdfFile.getAbsoluteFile}").run())
      })
      contents += new MenuItem(Action("Preferences")(showPreferenceDialog(frame)))
      contents += new MenuItem(Action("Exit"){frame.closeOperation()})
    }
    contents += new Menu("About") {
      contents += new MenuItem(Action("About")(showAboutDialog(frame)))
    }
  }
  frame.menuBar = menuB
  frame.open()
}

object Main{
  def main(args: Array[String]) {
    val devMode = args.exists(_ == "--dev")
    val prefs: Preferences =
      if(devMode) Preferences.userNodeForPackage(this.getClass).node("development")
      else        Preferences.userNodeForPackage(this.getClass)
    val app = AppMain(
      preferences = prefs
    )
  }
}