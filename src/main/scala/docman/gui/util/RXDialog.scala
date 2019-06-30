package docman.gui.util

trait RXDialog[T]{
  def build(initial: T): RControl[T]
}
