package docman.frontend.swing2

import java.awt.Component
import java.awt.event.{WindowEvent, WindowListener}
import java.nio.file.{Files, Path, Paths}

import cats.effect._
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import docman.backend.csv.CSVStore
import docman.core.Document
import docman.frontend.swing2.components._
import docman.utils.Logging
import javax.swing._
import monix.execution.Ack
import monix.reactive.subjects.{PublishSubject, ReplaySubject}
import monix.reactive.{Observable, _}
import net.miginfocom.swing.MigLayout

object Main extends CommandIOApp(
  name = "docman2-gui",
  header = "GUI for docman2",
  version = "0.0.x"
) with Logging {
  override def main: Opts[IO[ExitCode]] = {
    val dbfile = Opts.option[Path]("dbfile", "data base file")
      .withDefault(Paths.get(System.getProperty("user.home", "")).resolve(".docman2/db.0.csv"))
    val root = Opts.argument[Path]("root").validate("document root must be directory")(Files.isDirectory(_))
    val dbOpt = (root,dbfile map (_.toFile)).mapN(CSVStore.asResource[IO](_,_))

    dbOpt.map{db =>
      import monix.execution.Scheduler.Implicits.global

//      implicit val reporter: UncaughtExceptionReporter = monix.execution.UncaughtExceptionReporter.default
      val body: Resource[IO, Component] = for{
        store: CSVStore[IO] <- db

        _ <- Resource.liftF(store.reloadDB)

        (scanTrigger, scanUpdates: Observable[Seq[(store.Id, store.Doc)]]) <- Resource.make(IO{
          val s = PublishSubject[Unit]()
          val updts = s.mapEvalF(_ => for{
            found <- store.scanForPDFs
            _ <- IO(logger.info(s"found ${found.size} new documents"))
            newState <- store.getAllDocuments
          } yield newState)
          (s, updts, updts.subscribe())
        })(sc => IO{sc._3.cancel()}).map(x => (x._1,x._2))

        button <- rxbutton[IO](label = Observable("Scan"), scanTrigger)

        updates <- Resource.make(IO{
          val rs = ReplaySubject[(Path,Document)]()
          val s = rs.subscribe(u => store.updateDocument(u._1, u._2).map(_ => Ack.Continue).unsafeToFuture())
          (rs: Observable[(Path,Document)],s)
        })(rss => IO(rss._2.cancel())).map(_._1)

        selection <- PublishSubject[Path]().pure[Resource[IO,*]]

        table <- components.rxtable[IO,Path,Document](
          columns = IndexedSeq(
            rxtable.Column[Document,String]("Absender", _.sender.orEmpty, ((s: String) => (_: Document).copy(sender = s.some)).some, prefWidth = 100.some),
            rxtable.Column[Document,String]("Betreff", _.subject.orEmpty, prefWidth = 300.some),
            rxtable.Column[Document,String]("Datum", _.date.map(_.toString).orEmpty, prefWidth = 100.some),
            rxtable.Column[Document,String]("Tags", _.tags.toString),
            rxtable.Column[Document,String]("Seit", _.created.toString),
            rxtable.Column[Document,String]("Geändert", _.lastModified.toString)
          ),
          rows =
            Observable(
              (Observable(Observable.pure(()), updates.map(_ => ())).merge.mapEvalF(_ => store.getAllDocuments)),
              scanUpdates)
              .merge
              .flatMap(Observable.fromIterable)
              .map(_.map(_.some)),
          selection = selection,
          updates = Observer.dump("change")
        )

        pdfview <- pdfview[IO](selection.mapEvalF(store.access).map(Option(_)))

        tagview <- tagview[IO](Observable(Set("test","tags")), Observer.dump("selected tags"))

        panel <- Resource.liftF(IO{
          val p = new JPanel(new MigLayout())
          val tb  = new JToolBar()
          tb.setFloatable(false)
          tb.add(button)
          tb.addSeparator()
          tb.add(new JLabel("Suche"))
          tb.add(new JTextField(20))
          p.add(tb, "grow,wrap")
          val left = new JPanel(new MigLayout)
          left.add(table, "push, grow, wrap")
          left.add(tagview, "growx")
          val sp = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, left, pdfview)
          p.add(sp, "grow, push, wrap")
          p
        })
      } yield panel

      for{
        (r,fin) <- body.allocated
        frame = new JFrame("docman2")
        _ <- IO {
          //for faster pdf rendering
          System.setProperty("sun.java2d.cmm", "sun.java2d.cmm.kcms.KcmsServiceProvider")
          frame.getContentPane.add(r)
          frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
          frame.setVisible(true)
          frame.addWindowListener(new WindowListener {
            override def windowOpened(e: WindowEvent): Unit = ()
            override def windowClosing(e: WindowEvent): Unit = fin.unsafeRunSync()
            override def windowClosed(e: WindowEvent): Unit = ()
            override def windowIconified(e: WindowEvent): Unit = ()
            override def windowDeiconified(e: WindowEvent): Unit = ()
            override def windowActivated(e: WindowEvent): Unit = ()
            override def windowDeactivated(e: WindowEvent): Unit = ()
          })
        }
      } yield ExitCode.Success
    }
  }
}
