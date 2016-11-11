package docman

import java.awt.image.BufferedImage
import java.io.{File, FilenameFilter}
import java.util.prefs.Preferences
import javax.imageio.ImageIO
import javax.swing.RowFilter.Entry
import javax.swing.event.{ListSelectionEvent, ListSelectionListener}
import javax.swing.{Action => _, _}

import com.typesafe.scalalogging.slf4j.StrictLogging
import docman.components.TagView
import jiconfont.IconCode
import jiconfont.icons.Typicons
import jiconfont.swing.IconFontSwing
import migpanel.MigPanel
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observable, Subject}
import rxutils.swing.RxLabel

import scala.collection.immutable.IndexedSeq
import scala.swing.FileChooser.SelectionMode
import scala.swing._
import scala.swing.event.ValueChanged

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

case class AppMain(preferences: Preferences) extends Reactor {

  def buildIcon(icon: IconCode, size: Int = 20): Icon =
    IconFontSwing.buildIcon(icon, size)

  object actions {
    implicit class RichAction(val a: Action) {
      def withIcon(icon: IconCode): Action = {
        a.icon = buildIcon(icon,150)
        a.smallIcon = buildIcon(icon,20)
        a
      }
      def withDescription(d: String): Action = {
        a.longDescription = d
        a
      }
    }
    val closeApplication: Action = Action("Exit"){frame.closeOperation()}
      .withIcon(Typicons.POWER)

    val openSelectedPDF: Action = Action("Open Selected PDF"){
        table.getSelectedDocuments.headOption.foreach(selectedPDF =>
          sys.process.Process(f"gnome-open ${selectedPDF.pdfFile.getAbsoluteFile}").run())
      }
      .withIcon(Typicons.EYE)
      .withDescription("Open the PDF for the selected entry with an external viewer")

    val saveAllMeta: Action = Action("Save All Meta") {tableModel.saveAllMeta()}.withIcon(Typicons.FOLDER)
  }
  val applicationTitle = "Docman2"

  val dbDirs: Subject[Set[File]] = BehaviorSubject(
    preferences.get("db.dirs","").split(";").map(new File(_)).filter(_.exists()).toSet
  )

  dbDirs.foreach{ dirs =>
    preferences.put("db.dirs", dirs.mkString(";"))
  }

  val pdfs: Observable[IndexedSeq[File]] = dbDirs.map { dirs =>
      (for {
        dir <- dirs
        pdf <- dir.listFiles(new FilenameFilter {
          def accept(dir: File, name: String): Boolean = name.endsWith("pdf")
        })
      } yield pdf).toIndexedSeq.sortBy(_.toString)
    }

  val docs: Observable[IndexedSeq[Doc]] = pdfs.map { _ map Doc.fromFile }

  val tableModel: DocumentTableModel = DocumentTableModel(docs.toBlocking.first, DProp.ALL)
  docs.foreach(tableModel.setDocs)

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
        dbDirs.onNext(Set(fc.selectedFile))
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
      panel.add(new Label(VersionInfo.version),"wrap")
      contents = panel
    }
    dia.pack()
    dia.resizable = false
    dia.open()
  }

  val table: DocumentTable = new DocumentTable(tableModel)

  val selectedDocuments = BehaviorSubject(Set[Doc]())

  table.getSelectionModel.addListSelectionListener(new ListSelectionListener {
    def valueChanged(e: ListSelectionEvent): Unit = {
      if(!e.getValueIsAdjusting)
        selectedDocuments.onNext(table.getSelectedDocuments.toSet)
    }
  })

  val displayedPdf: Observable[Option[File]] = selectedDocuments.map{
    case selected if selected.size == 1 => Some(selected.head.pdfFile)
    case _ => None
  }

  val viewer: MigPanel = PDFViewer.newViewer(displayedPdf)

  val quickSearchBar: TextField = new TextField(30)
  this.listenTo(quickSearchBar)
  reactions += {
    case ValueChanged(_) => table.getRowSorter.asInstanceOf[DefaultRowSorter[DocumentTableModel,Int]].setRowFilter(
      new RowFilter[DocumentTableModel,Int]{
        def include(entry: Entry[_ <: DocumentTableModel, _ <: Int]): Boolean =
          (0 until entry.getModel.getColumnCount).exists(c => entry.getStringValue(c).toLowerCase.contains(quickSearchBar.text.toLowerCase))
      }
    )
  }

  private val toolbar: JToolBar = new JToolBar("Main Toolbar")

  toolbar.add(new JButton(actions.openSelectedPDF.peer))
  toolbar.addSeparator()
  toolbar.add(new JLabel("Search",buildIcon(Typicons.ZOOM),0))
  toolbar.add(quickSearchBar.peer)

  val leftPane = new MigPanel("fill","","[10]10[fill]")
  val scrollPane: ScrollPane = new ScrollPane(Component.wrap(table))
  leftPane.add(scrollPane,"grow,pushy, span 2,wrap")
  val tags: rx.lang.scala.Observable[Seq[(String,Int)]] = {

    val allTags: rx.lang.scala.Observable[Iterable[String]] = docs.map(_.flatMap(_.properties.get(TagListDP)).flatten)
    allTags.map{_.groupBy(identity).toSeq.map{case (x,y) => (x,y.size)}}
  }
  val foo: Unit = tags.foreach(println)
  leftPane.add(TagView(tags))

  //the main split pane, with table left and pdf viewer right
  private val splitPane: SplitPane = new SplitPane(Orientation.Vertical, leftPane, viewer)
  splitPane.resizeWeight = 0.6

  val mainPanel = new MigPanel()
  mainPanel.add(Component.wrap(toolbar), "growx,wrap")
  mainPanel.add(splitPane, "grow,push")

  val frame = new MainFrame{
    val icon: BufferedImage = ImageIO.read(this.getClass.getClassLoader.getResourceAsStream("images/app_icon.png"))
    iconImage = icon
    title = f"Docman - ${VersionInfo.version}" + (if(VersionInfo.isDefVersion) " : development mode" else "")

    minimumSize = new Dimension(800,600)
    contents = mainPanel

    override def closeOperation() {
      visible = true
      tableModel.saveAllMeta()
      super.closeOperation()
    }
  }


  val menuB = new MenuBar {
    contents += new Menu("File") {

      contents += new MenuItem(actions.saveAllMeta)
      contents += new MenuItem(actions.openSelectedPDF)
      contents += new MenuItem(Action("Preferences")(showPreferenceDialog(frame)))
      contents += new MenuItem(actions.closeApplication)
    }
    contents += new Menu("About") {
      contents += new MenuItem(Action("About")(showAboutDialog(frame)))
    }
  }
  frame.menuBar = menuB
  frame.open()
}

object Main extends StrictLogging {
  def main(args: Array[String]) {
    IconFontSwing.register(Typicons.getIconFont)

    val devMode = VersionInfo.isDefVersion
    if (devMode) logger.warn( "running in developer mode" )
    val prefs: Preferences =
      if(devMode) Preferences.userNodeForPackage(this.getClass).node("development")
      else        Preferences.userNodeForPackage(this.getClass)
    AppMain(
      preferences = prefs
    )
  }
}