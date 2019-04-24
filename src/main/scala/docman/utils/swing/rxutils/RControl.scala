package docman.utils.swing.rxutils

import rx.lang.scala.Observable

import scala.swing.Component

case class RControl[T](component: Component, obs: Observable[T])

