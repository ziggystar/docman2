package docman.gui

import java.awt.image.BufferedImage
import java.io.File
import java.sql.Date
import java.util.prefs.Preferences

import cats.data.EitherT
import cats.effect.IO
import com.typesafe.scalalogging.StrictLogging
import docman.VersionInfo
import docman.config.Config
import docman.core.{AuthorDP, DProp, DateDP, DirectSidecarPersistable, Doc, Document, Persistable, PropertyMap, RODocumentStore, SubjectDP, TagListDP}
import docman.engine.SideCarRO
import docman.gui.dialogs.PreferenceDialog
import docman.gui.table.{DocumentTable, DocumentTableModel}
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
import rx.lang.scala.{Observable, Subscription}

import scala.swing._

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

case class AppMain(preferences: Preferences) extends Reactor with StrictLogging {

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

  private val initialConfig: Config = Config.load(preferences)
    .left.map(failure => {logger.warn("failed to read config", failure); Config()})
    .merge

  val config: BehaviorSubject[Config] = BehaviorSubject[Config](initialConfig)

  val cfgPrint: Subscription = config.subscribe(c => logger.debug("config changed" + c))

  val persistConfig: Subscription = config.distinctUntilChanged.subscribe(Config.store(preferences,_))

  /** Given a directory, list all files recursively. */
  def recursiveFiles(file: File): Array[File] = if(file.isDirectory){
    file.listFiles().flatMap(recursiveFiles)
  } else Array(file)

  val backend: Observable[BackendAdapter] =
    config.distinctUntilChanged
      .map(c => new SideCarRO(c.searchDirs.map(sd => sd.dir -> sd.recursive)))
      .map(BackendAdapter)

  val docs: Observable[IndexedSeq[Doc]] = backend.flatMap(_.docStream())

  val tableModel: DocumentTableModel = DocumentTableModel(docs.toBlocking.first, DProp.ALL, DirectSidecarPersistable.docPersistable)
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
      contents += new MenuItem(Action("Preferences")(
        PreferenceDialog.show(frame,config.toBlocking.first).subscribe(config.onNext(_)))
      )
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

object Main extends StrictLogging {
  def main(args: Array[String]) {
    IconFontSwing.register(Typicons.getIconFont)

    val devMode = VersionInfo.isDefVersion
    if (devMode) logger.warn( "running in developer mode" )
    val prefs: Preferences =
      if(devMode) Preferences.userNodeForPackage(this.getClass).node("development")
      else        Preferences.userNodeForPackage(this.getClass)
    AppMain(preferences = prefs)
  }
}

case class BackendAdapter(backend: RODocumentStore[EitherT[IO,String,?]]{type Id = File}){
  def documentToDoc(file: File, document: Document): Doc = {
    val pm = DProp.ALL.foldLeft(PropertyMap.empty){ case (map, dp) =>
      if(dp.name == DateDP.name) {
        document.date.map(d => map.put(DateDP)(new Date(d.toEpochDay))).getOrElse(map)
      }
      else if(dp.name == AuthorDP.name) {
        document.sender.map(d => map.put(AuthorDP)(d)).getOrElse(map)
      }
      else if(dp.name == SubjectDP.name) {
        document.subject.map(d => map.put(SubjectDP)(d)).getOrElse(map)
      }
      else if(dp.name == TagListDP.name) {
        map.put(TagListDP)(document.tags)
      }
      else
      {
        map
      }
    }
    Doc(file, pm)
  }

  def docStream(reload: Observable[Unit] = Observable.just(())) : Observable[IndexedSeq[Doc]] = {
    reload.map(_ => backend
      .getDocuments
      .value
      .unsafeRunSync()
      .getOrElse(Seq())
      .map((documentToDoc _).tupled)(collection.breakOut))
  }

  def persistable: Persistable[Doc] = ???
}