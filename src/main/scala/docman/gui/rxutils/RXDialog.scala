package docman.gui.rxutils

trait RXDialog[T]{
  def build(initial: T): RControl[T]
}
