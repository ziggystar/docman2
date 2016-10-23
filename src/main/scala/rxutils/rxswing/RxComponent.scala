package rxutils.rxswing

import rx.lang.scala.Observable

/** Base trait for the GUI framework.
  */
trait RxBlock {
  /** The type of the State of this component. Should be minimal and complete, i.e. do not preprocess. */
  type S
  type P

  def state: Observable[S]
}

sealed trait STree[+X]
object SNil
case class SCons[+H,+T](head: H, tail: STree[T])
case class SGroup[+L,+R](left: STree[L], right: STree[R])
