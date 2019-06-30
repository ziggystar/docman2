package docman.frontend.swing.util

import rx.lang.scala.Observable

import scala.swing.Component

case class RControl[T](component: Component, obs: Observable[T])

