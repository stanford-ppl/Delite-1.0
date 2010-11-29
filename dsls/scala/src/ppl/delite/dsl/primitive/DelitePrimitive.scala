package ppl.delite.dsl.primitive

import ppl.delite.core.DeliteDSLType
import ppl.delite.dsl.primitive.DeliteInt.ProxyFactory

object DelitePrimitive {
  def apply(p: Int)    = DeliteInt(p)
  def apply(p: Short)  = DeliteShort(p)
  def apply(p: Long)   = DeliteLong(p)
  def apply(p: Double) = DeliteDouble(p)
  def apply(p: Float)  = DeliteFloat(p)
  def apply(p: Char)   = DeliteChar(p)
  def apply(p: Byte)   = DeliteByte(p)
}


trait DelitePrimitive[T] extends Cloneable {
  def value: T

  override def toString = value.toString
}