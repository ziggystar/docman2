package docman.frontend.jfx

import javafx.beans.binding.{Bindings, ObjectBinding}
import javafx.beans.value.ObservableValue
import javafx.util.Callback

package object util {
  implicit def functionToCallback[A,B](f: A => B): Callback[A,B] = (param: A) => f(param)
  implicit class RichObservable[A](val v: ObservableValue[A]) extends AnyVal {
    def map[B](f: A => B): ObjectBinding[B] = Bindings.createObjectBinding(() => f(v.getValue), v)
  }
}
