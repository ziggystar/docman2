package docman.components


import java.awt.Color
import javax.swing.BorderFactory
import javax.swing.border.BevelBorder

import rx.lang.scala._

import scala.swing.{FlowPanel, Label}

/**
  * Created by thomas on 08.11.16.
  */
case class TagView(tags: Observable[Seq[(String,Int)]], maxTags: Int = 10) extends FlowPanel {
  val displayedTags: Observable[Seq[(String,Int)]] = tags.map(_.sortBy(_._2).take(maxTags))

  def makeLabel(tag: String, count: Int): Label = {
    val l = new Label(tag)
    l.peer.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED))
    l.peer.getInsets.set(9,9,9,9)
    l
  }

  val subscription: Subscription = displayedTags.subscribe { newTags =>
    this.contents ++= newTags.map((makeLabel _).tupled)
  }
}
