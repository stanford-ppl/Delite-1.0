package ppl.apps.ml.baseline.collection


import java.util.BitSet

object BooleanVector {

  def apply(l: Int) = new BooleanVector(l)
}

class BooleanVector(l: Int) extends Vector {

  def this() = this(0)

  val elements = new BitSet(l)

  var length   = l;

  var isRow = true

  var offset = 0
  var stride = 1

  def +=(b: Boolean) {
    elements.set(length, b)
    length += 1
  }

  def apply(idx: Int) = {
    elements.get(idx)
  }

  def update(idx: Int, b: Boolean) = {
    if(idx >= length) length = idx + 1
    elements.set(idx, b)
  }

  override def toString = {
    val str = new StringBuilder(super.toString)
    str.append("\n[ ")
    var idx = 0
    while(idx != length) {
      str.append(elements.get(idx).toString + " ")
      if(isRow == false)
        str.append("\n  ")
      idx +=1
    }
    str.append("]")
    str.toString
  }

}