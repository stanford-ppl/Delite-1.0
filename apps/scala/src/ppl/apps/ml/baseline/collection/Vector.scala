package ppl.apps.ml.baseline.collection

abstract class Vector {

  def length: Int
  def isRow: Boolean

  def offset: Int
  def stride: Int

  private[collection] def chkBounds(i: Int) {
    if(i < 0 || i >= length) throw new IllegalArgumentException("Vector index out of bounds")
  }

  private[collection] def chkBounds(v1: Vector, v2: Vector) {
    if(v1.length != v2.length) throw new IllegalArgumentException("Vectors must have equal length")
  }

}
