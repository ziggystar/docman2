package docman.frontend.swing2


import java.awt.Component
import java.awt.event.{WindowEvent, WindowListener}
import java.nio.file.{Files, Path, Paths}

import cats.effect._
import cats.implicits._
import com.monovore.decline._
import com.monovore.decline.effect._
import com.typesafe.scalalogging.StrictLogging
import javax.swing._
import components._
import docman.backend.csv.CSVStore
import docman.core.Document
import docman.frontend.swing.util.MigPanel
import monix.execution.{Ack, UncaughtExceptionReporter}
import monix.reactive.subjects.{ReplaySubject, Var}
import monix.reactive.{Observable, _}
import net.miginfocom.swing.MigLayout



object Main extends CommandIOApp(
  name = "docman2-gui",
  header = "GUI for docman2",
  version = "0.0.x"
) with StrictLogging {
  override def main: Opts[IO[ExitCode]] = {
    val dbfile = Opts.option[Path]("dbfile", "data base file")
      .withDefault(Paths.get(System.getProperty("user.home", "")).resolve(".docman2/db.0.csv"))
    val root = Opts.argument[Path]("root").validate("document root must be directory")(Files.isDirectory(_))
    val dbOpt = (root,dbfile map (_.toFile)).mapN(CSVStore.asResource[IO](_,_))

    dbOpt.map{db =>
      import monix.execution.Scheduler.Implicits.global

      implicit val reporter: UncaughtExceptionReporter = monix.execution.UncaughtExceptionReporter.default
      val body: Resource[IO, Component] = for{
        store <- db

        _ <- Resource.liftF(store.reloadDB)

        initial <- Resource.liftF(store.getAllDocuments)
        _ <- Resource.liftF(IO(logger.info("initial: " + initial)))

        updates <- Resource.make(IO{
          val rs = ReplaySubject[(Path,Document)]()
          val s = rs.subscribe(u => store.updateDocument(u._1, u._2).map(_ => Ack.Continue).unsafeToFuture)
          (rs: Observable[(Path,Document)],s)
        })(rss => IO(rss._2.cancel())).map(_._1)

        table <- components.rxtable[IO,Path,Document](
          columns = IndexedSeq(rxtable.Column("Absender"), rxtable.Column("Betreff")),
          rows = (Observable(initial) ++ updates.mapEvalF(_ => store.getAllDocuments)).map(_.toIndexedSeq)
        )

        button <- rxbutton[IO](label = Observable("Scan"), Observer.empty)
        panel <- Resource.liftF(IO{
          val p = new JPanel(new MigLayout())
          p.add(table)
          p.add(button)
          p
        })
      } yield panel

      for{
        (r,fin) <- body.allocated
        frame = new JFrame("docman2")
        _ <- IO {
          frame.getContentPane.add(r)
          frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
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
