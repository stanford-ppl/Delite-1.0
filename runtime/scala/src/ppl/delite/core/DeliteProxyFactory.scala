package ppl.delite.core

/**
 * This is a generic interface that allows Delite to instantiate a new Proxy of type T without understanding what T is
 */
trait DeliteProxyFactory[T <: DeliteDSLType] {
  def newProxy : T
}