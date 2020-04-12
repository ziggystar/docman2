package docman.frontend.swing2.components

import java.awt.Component

import cats.effect.{Resource, Sync}
import javax.swing.JLabel
import monix.reactive.{Observable, Observer}


object tagview {
  def apply[F[_]: Sync](tags: Observable[Set[String]], selection: Observer[Set[String]]): Resource[F,Component] =
    Resource.pure(new JLabel("tag-view not implemented"))
}
