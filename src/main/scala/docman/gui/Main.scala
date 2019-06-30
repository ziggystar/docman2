package docman.gui

import java.awt.image.BufferedImage
import java.io.File
import java.nio.file.{Files, Path, Paths}

import com.typesafe.scalalogging.StrictLogging
import docman.{BuildInfo, VersionInfo}
import docman.core.{DProp, DirectSidecarPersistable, Doc, TagListDP}
import docman.engine.csv.CSVStore
import docman.gui.table.{DocumentTable, DocumentTableModel}
import docman.utils.Isomorphism
import docman.utils.swing.pdf.PDFViewer
import docman.utils.swing.rxutils.RControl
import docman.utils.swing.{MigPanel, TagView, _}
import javax.imageio.ImageIO
import javax.swing.RowFilter.Entry
import javax.swing.event.ListSelectionEvent
import javax.swing.{Action => _, _}
import jiconfont.icons.Typicons
import jiconfont.swing.IconFontSwing
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observable, Subject, Subscription}

import scala.swing._

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

case class Config(searchDir: Path, dbFile: File)

case class AppMain(config: Config) extends Reactor with StrictLogging {

  IconFontSwing.register(Typicons.getIconFont)

  object actions {
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

  val store = CSVStore(config.searchDir, config.dbFile)
  val backend: BackendAdapter[Path] = BackendAdapter[Path](store, Isomorphism[Path,File](_.toFile, _.toPath))

  val persistCalls: Subject[Doc] = Subject()

  val updates: Subscription = persistCalls.subscribe(backend.persistable.persist(_))

  val docs: Observable[IndexedSeq[Doc]] = backend.docStream()

  val tableModel: DocumentTableModel = DocumentTableModel(docs.toBlocking.first, DProp.ALL, persistCalls.onNext)
  docs.foreach(tableModel.setDocs)

  def showAboutDialog(owner: Window): Unit = {
    val dia = new swing.Dialog(owner){
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

  table.getSelectionModel.addListSelectionListener((e: ListSelectionEvent) => {
    if (!e.getValueIsAdjusting)
      selectedDocuments.onNext(table.getSelectedDocuments.toSet)
  })

  val displayedPdf: Observable[Option[File]] = selectedDocuments.map{
    case selected if selected.size == 1 => Some(selected.head.pdfFile)
    case _ => None
  }

  val viewer: MigPanel = PDFViewer.newViewer(displayedPdf)

  val quickSearchBar: RControl[String] = textField("", 24)

  private val toolbar: JToolBar = new JToolBar("Main Toolbar")

  toolbar.add(new JButton(actions.openSelectedPDF.peer))
  toolbar.addSeparator()
  toolbar.add(new JLabel("Search", buildIcon(Typicons.ZOOM),0))
  toolbar.add(quickSearchBar.component.peer)

  val leftPane = new MigPanel("fill","","[10]10[fill]")
  val scrollPane: ScrollPane = new ScrollPane(Component.wrap(table))
  leftPane.add(scrollPane,"grow, pushy, wrap")

  val tags: Observable[Seq[(String,Int)]] = {
    val allTags: Observable[Iterable[String]] = docs.map(_.flatMap(_.properties.get(TagListDP)).flatten)
    allTags.map{_.groupBy(identity).toSeq.map{case (x,y) => (x,y.size)}}
  }
  private val tagView = TagView(tags)
  leftPane.add(tagView)

  private val searchParams: Observable[(String, Set[String])] = quickSearchBar.obs.combineLatest(tagView.selectecTags)
  val rowFilter: Observable[RowFilter[DocumentTableModel,Int]] = searchParams.map{
    case (search, selectedTags) =>
      (entry: Entry[_ <: DocumentTableModel, _ <: Int]) => {
        def searchHit = search.isEmpty ||
          (0 until entry.getModel.getColumnCount).exists { rowIndex =>
            entry.getStringValue(rowIndex).toLowerCase.contains(search.toLowerCase)
          }

        def tagHit = selectedTags.isEmpty || {
          val entryTags: Set[String] = Option(entry.getValue(entry.getModel.findColumn("Tags")).asInstanceOf[Set[String]]).getOrElse(Set[String]())
          selectedTags.subsetOf(entryTags)
        }

        tagHit && searchHit
      }
  }

  //apply row filter to out table
  val filterSubscription: Subscription = rowFilter.subscribe { rf =>
    table.getRowSorter.asInstanceOf[DefaultRowSorter[DocumentTableModel, Int]].setRowFilter(rf)
  }

  //the main split pane, with table left and pdf viewer right
  private val splitPane: SplitPane = new SplitPane(Orientation.Vertical, leftPane, viewer)
  splitPane.resizeWeight = 0.6

  val mainPanel = new MigPanel()
  mainPanel.add(Component.wrap(toolbar), "growx,wrap")
  mainPanel.add(splitPane, "grow,push")

  val frame: MainFrame = new MainFrame{
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


  val menuB: MenuBar = new MenuBar {
    contents += new Menu("File") {
      contents += new MenuItem(actions.saveAllMeta)
      contents += new MenuItem(actions.openSelectedPDF)
      contents += new MenuItem(actions.closeApplication)
    }
    contents += new Menu("About") {
      contents += new MenuItem(Action("About")(showAboutDialog(frame)))
    }
  }
  frame.menuBar = menuB
  frame.pack()
  frame.open()
}

import cats.implicits._
import com.monovore.decline._

object Main extends CommandApp(
  name="docman2",
  header = "PDF management application",
  main = {
    val db = Opts.option[String]("dbfile", "data base file")
      .withDefault(System.getProperty("user.home", "") + "/.docman2/db.0.csv")
      .map(new File(_))
    val root = Opts.argument[String]("root").map(Paths.get(_))
    (root,db).mapN(Config(_,_)).map(AppMain(_))
  },
  version = BuildInfo.version
)
