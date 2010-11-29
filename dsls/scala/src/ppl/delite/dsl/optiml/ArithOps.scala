package ppl.delite.dsl.optiml

trait ArithOps[T] {
  def +=(a: T, b: T) : Unit
  def +(a: T, b: T) : T
  def -(a: T, b: T) : T
  def *(a: T, b: T) : T
  def /(a: T, b: T) : T
  def zero : T
  def unary_-(a: T) : T
  def abs(a: T) : T
  def exp(a: T) : T
  def >(a: T, b: T) : T
  def <(a: T, b: T) : T
}
