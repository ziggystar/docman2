package docman.utils.swing.rxutils

trait RXDialog[T]{
  def build(initial: T): RControl[T]
}
