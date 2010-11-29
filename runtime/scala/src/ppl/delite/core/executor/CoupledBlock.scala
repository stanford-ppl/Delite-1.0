package ppl.delite.core.executor



trait CoupledBlock[T] extends ActiveBlock[T] {
  def submit(v: T) = process(v)
}