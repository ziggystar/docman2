package docman.frontend.swing.util

trait RXDialog[T]{
  def build(initial: T): RControl[T]
}
