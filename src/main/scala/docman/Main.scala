package docman

import java.awt.image.BufferedImage
import java.io.File
import java.util.prefs.Preferences

import com.typesafe.scalalogging.StrictLogging
import docman.ReactiveControls.dialogs
import docman.components.pdf.PDFViewer
import docman.components.table.{DocumentTable, DocumentTableModel}
import docman.components.{MigPanel, TagView}
import docman.core.{DProp, Doc, TagListDP}
import docman.utils.VersionInfo
import javax.imageio.ImageIO
import javax.swing.{Action => _, _}
import javax.swing.RowFilter.Entry
import javax.swing.event.ListSelectionEvent
import jiconfont.IconCode
import jiconfont.icons.Typicons
import jiconfont.swing.IconFontSwing
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Observable, Subject, Subscription}

import scala.swing._
import scala.util.Try

/**
 * @author Thomas Geier
 * @since 9/8/13
 */

case class AppMain(preferences: Preferences) extends Reactor with StrictLogging {

  def buildIcon(icon: IconCode, size: Int = 20): Icon =
    IconFontSwing.buildIcon(icon, size)

  object actions {
    implicit class RichAction(val a: Action) {
      def withIcon(icon: IconCode): Action = {
        a.icon = buildIcon(icon,size = 150)
        a.smallIcon = buildIcon(icon)
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

  /** Given a directory, list all files recursively. */
  def recursiveFiles(file: File): Array[File] = if(file.isDirectory){
    file.listFiles().flatMap(recursiveFiles)
  } else Array(file)

  val pdfs: Observable[IndexedSeq[File]] = dbDirs.distinctUntilChanged.map { dirs =>
      (for {
        dir <- dirs
        pdf <- recursiveFiles(dir).filter(_.getName.endsWith(".pdf"))
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
    val dirList = dialogs.FileList.build(dbDirs.toBlocking.first.toSeq.map(_ -> false))

    panel.add(new Label("Library Paths"), "wrap")
    panel.add(dirList.component, "wrap")
    val btnOk = ReactiveControls.button("Ok")
    val btnCancel = ReactiveControls.button("Cancel")
    panel.add(btnOk.component, "tag ok, split 2")
    panel.add(btnCancel.component, "tag cancel")
    dirList.obs.sample(btnOk.obs).subscribe{dirs =>
      println("saving config " + dirs.mkString(";"))
      dbDirs.onNext(dirs.map(_._1).toSet)
      dia.close()
    }
    btnCancel.obs.subscribe(_ => dia.close())
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

  table.getSelectionModel.addListSelectionListener((e: ListSelectionEvent) => {
    if (!e.getValueIsAdjusting)
      selectedDocuments.onNext(table.getSelectedDocuments.toSet)
  })

  val displayedPdf: Observable[Option[File]] = selectedDocuments.map{
    case selected if selected.size == 1 => Some(selected.head.pdfFile)
    case _ => None
  }

  val viewer: MigPanel = PDFViewer.newViewer(displayedPdf)

  val quickSearchBar: ReactiveControls.RControl[String] = ReactiveControls.textField("", 24)

  private val toolbar: JToolBar = new JToolBar("Main Toolbar")

  toolbar.add(new JButton(actions.openSelectedPDF.peer))
  toolbar.addSeparator()
  toolbar.add(new JLabel("Search",buildIcon(Typicons.ZOOM),0))
  toolbar.add(quickSearchBar.component.peer)

  val leftPane = new MigPanel("fill","","[10]10[fill]")
  val scrollPane: ScrollPane = new ScrollPane(Component.wrap(table))
  leftPane.add(scrollPane,"grow,pushy, span 2,wrap")

  val tags: Observable[Seq[(String,Int)]] = {
    val allTags: Observable[Iterable[String]] = docs.map(_.flatMap(_.properties.get(TagListDP)).flatten)
    allTags.map{_.groupBy(identity).toSeq.map{case (x,y) => (x,y.size)}}
  }
  private val tagView = TagView(tags)
  leftPane.add(tagView)

  private val searchParams: Observable[(String, Set[String])] = quickSearchBar.obs.combineLatest(tagView.selectecTags)
  val rowFilter: Observable[RowFilter[DocumentTableModel,Int]] = searchParams.map{
    case (search, selectedTags) =>
      println("new search params: " + (search,selectedTags))
      new RowFilter[DocumentTableModel,Int]{
        def include(entry: Entry[_ <: DocumentTableModel, _ <: Int]): Boolean = {
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
  }

  //apply row filter to out table
  val filterSubsription: Subscription = rowFilter.subscribe { rf =>
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