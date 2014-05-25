package rxutils.swing

import rx._
import scala.swing.{ComboBox, Label}
import utils.StringIso
import scala.swing.event.SelectionChanged

/**
 * @author Thomas Geier
 * @since 5/25/14
 */
class RxLabel[A](x: Rx[A]) extends Label(x.now.toString){
  val updater = Obs(x)(this.text = x().toString)
}

class RxComboBox[A](x: Var[A], items: Seq[A], stringIso: StringIso[A]) extends ComboBox[String](items.map(stringIso.forward)){
  val updater = Obs(x)(this.selection.item = stringIso.forward(x()))
  //register change listener
  this.selection.reactions += {
    case SelectionChanged(src) =>
      println(src)
      x.update(stringIso.backward(this.selection.item))
  }
}