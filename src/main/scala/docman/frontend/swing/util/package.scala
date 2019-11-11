package docman.frontend.swing

import javax.swing.{Action => _, _}
import jiconfont.IconCode
import jiconfont.swing.IconFontSwing
import monix.reactive.Observable

import scala.swing.{Action, Label}

package object util {

  def buildIcon(icon: IconCode, size: Int = 20): Icon =
    IconFontSwing.buildIcon(icon, size)

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
  def label(x: Observable[String]): Label = new Label("not implemented")
}
