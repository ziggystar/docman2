package rxutils.swing

import rx.lang.scala.Observable

import scala.swing.Label

/**
 * @author Thomas Geier
 * @since 5/25/14
 */
class RxLabel[A](x: Observable[A]) extends Label{
  x.map(_.toString).foreach(this.text = _)
}
