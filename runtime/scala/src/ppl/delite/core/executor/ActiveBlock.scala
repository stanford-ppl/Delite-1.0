package ppl.delite.core.executor

/**
 * The ActiveBlock is the base class for all components of our scheduler architecture
 */
abstract class ActiveBlock[T] {
  def submit(v: T)
  def process(v: T)
}