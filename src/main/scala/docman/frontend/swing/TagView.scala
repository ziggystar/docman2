package docman.frontend.swing

import java.awt.Color
import java.awt.event.{MouseAdapter, MouseEvent}

import cats.effect.IO
import javax.swing.BorderFactory
import javax.swing.border.BevelBorder
import monix.execution.Ack
import monix.reactive.Observable
import monix.reactive.subjects.{PublishSubject, Subject}
import monix.execution.Scheduler.Implicits.global
import wraplayout.WrapLayout

import scala.swing.{Dimension, FlowPanel, Label}

/**
  * Created by thomas on 08.11.16.
  */
case class TagView(tags: Observable[Map[String,Int]], maxTags: Int = 20) extends FlowPanel {

  peer.setLayout(new WrapLayout())

  preferredSize = new Dimension(300,100)

  val displayedTags: Observable[Seq[(String,Int)]] = tags.map(_.toSeq.sortBy(-_._2).take(maxTags))
  val toggles: PublishSubject[(String,Boolean)] = PublishSubject[(String,Boolean)]
  val selectecTags: Observable[Set[String]] = toggles.scan(Set[String]()){
    case (ts, (t,true)) => ts + t
    case (ts, (t,false)) => ts - t
  }

  def makeLabel(tag: String, count: Int): Label = {
    val l = new Label(tag){
      var toggled: Boolean = false
    }
    l.peer.setForeground(Color.GRAY)
    l.peer.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED))
    l.peer.getInsets.set(9,9,9,9)
    l.peer.addMouseListener(new MouseAdapter {
      override def mouseClicked(e: MouseEvent): Unit = {
        l.toggled = !l.toggled
        toggles.onNext(tag -> l.toggled)
        l.peer.setForeground(if(l.toggled) Color.BLACK else Color.GRAY)
      }
    })
    l
  }

  val subscription = displayedTags.subscribe { newTags =>
    (IO(this.contents ++= newTags.map((makeLabel _).tupled)).map(_ => Ack.Continue).unsafeToFuture())
  }
}
