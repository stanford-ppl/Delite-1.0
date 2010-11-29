package ppl.apps.ml.baseline.collection

object IntVector {

/**
   * Constructors
   */
  def apply(l: Int): IntVector = {
    val elms = new Array[Int](l)
    new IntVector(elms, l)
  }

  def apply(elms: Array[Int], length: Int) = new IntVector(elms, length)
  def apply(elms: Array[Int], is_row: Boolean, length: Int) = new IntVector(elms, is_row, length)
  def apply(elms: Array[Int], offset: Int, stride: Int, length: Int) = new IntVector(elms, offset, stride, length)
  def apply(elms: Array[Int], is_row: Boolean, offset: Int, stride: Int, length: Int) = new IntVector(elms, is_row, offset, stride, length)

  def indexedFromFunction(l: Int,  f: Int => Int): IntVector = {
    val _elms = new Array[Int](l)
    var idx = 0
    while (idx < l) {
      _elms(idx) = f(idx)
      idx += 1
    }
    IntVector(_elms,l)
  }

}

class IntVector(elms: Array[Int], is_row: Boolean, _offset: Int, _stride: Int, _length: Int   ) extends Vector {

  def this(elms: Array[Int], length: Int) = this(elms, true, 0 ,1 ,length)
  def this(elms: Array[Int], is_row: Boolean, length: Int) = this(elms, is_row, 0, 1, length)
  def this(elms: Array[Int], offset: Int, stride: Int, length: Int) = this(elms, true, offset, stride, length)

  private[collection] var elements: Array[Int] = elms
  var length: Int = _length

  var isRow = is_row

  var offset = _offset
  var stride: Int = _stride

  def pprint {
    val str = new StringBuilder(super.toString)
    str.append("\n[ ")
    var idx = 0
    while(idx != length) {
      str.append(elements(idx).toString + " ")
      if(isRow == false)
        str.append("\n  ")
      idx +=1
    }
    str.append("]")
    str.toString
    println(str)
  }

  def +(v: IntVector) = {
    chkBounds(this, v)
    val end = length
    val elms = new Array[Int](end)
    val v1off = offset
    val v1str = stride
    val v2off = v.offset
    val v2str = v.stride
    var idx = 0
    while(idx != end) {
      elms(idx) = this.elements(v1off + idx*v1str) + v.elements(v2off + idx*v2str)
      idx += 1
    }
    IntVector(elms, end)
  }

  /**
   * Vector Division by a single Int. All elements are divided by this Int and a new IntVector is returned
   */
  def /(d: Int) = {
    val end = length
    val elms = new Array[Int](end)
    val voff = offset
    val vstr = stride
    var idx = 0
    while(idx != end) {
      elms(idx) = this.elements(voff + idx*vstr) / d
      idx += 1
    }
    IntVector(elms, end)
  }

  /**
   * Vector Subtraction
   */
  def -(v: IntVector) = {
    chkBounds(this, v)
    val end = length
    val elms = new Array[Int](end)
    val v1off = offset
    val v1str = stride
    val v2off = v.offset
    val v2str = v.stride
    var idx = 0
    while(idx != end) {
      elms(idx) = this.elements(v1off + idx*v1str) - v.elements(v2off + idx*v2str)
      idx += 1
    }
    IntVector(elms, end)
  }

  /**
   * Outer Product
   */
  def outer(v: IntVector) = {
    chkBounds(this, v)
    val lgt = length
    val elms = new Array[Int](lgt * lgt)
    val v1off = offset
    val v1str = stride
    val v2off = v.offset
    val v2str = v.stride
    var i = 0
    while(i != lgt) {
      var j = 0
      while(j != lgt) {
        elms(i*lgt+j) = this.elements(v1off + i*v1str) * v.elements(v2off + j*v2str)
        j += 1
      }
      i += 1
    }
    IntMatrix(elms, lgt, lgt)
  }

  /**
   * Vector multiplication by a single Int
   */
  def *(d: Int) = {
    val end = length
    val elms = new Array[Int](end)
    val voff = offset
    val vstr = stride
    var idx = 0
    while (idx != end) {
      elms(idx) = this.elements(voff + idx*vstr) * d
      idx += 1
    }
    IntVector(elms, end)
  }

  /**
   * Inner Product
   */
  def dot(v: IntVector) = {
    chkBounds(this, v)
    val end = length
    val v1off = offset
    val v2off = v.offset
    val v1str = stride
    val v2str = v.stride
    var idx = 0
    var acc = 0.0
    while (idx != end) {
      acc += (this.elements(v1off + idx*v1str) * v.elements(v2off + idx*v2str))
      idx += 1
    }
    acc
  }

  /**
   *   Transposing Vector
   */
  def trans = {
    val end = length
    val elms = new Array[Int](end)
    val voff = offset
    val vstr = stride
    var i = 0
    while (i != end) {
      elms(i) = this.elements(voff + i*vstr)
      i += 1
    }
    IntVector(elms, !isRow, end)
  }

  /**
   * Transposing Mutable Vector
   */
  def trans_m = {
    isRow = !isRow
  }

  def map(f: Int => Int) = {
    val end = length
    val elms = new Array[Int](end)
    val voff = offset
    val vstr = stride
    var idx = 0
    while (idx != end) {
      elms(idx) = f(this.elements(voff + idx*vstr))
      idx += 1
    }
    IntVector(elms, isRow, length)
  }

  def sum = {
    val end = length
    val voff = offset
    val vstr = stride
    var acc = 0.0
    var i = 0
    while (i != end) {
      acc += this.elements(voff + i*vstr)
      i += 1
    }
    acc
  }

  def replicateMatrix(m: Int, n: Int): IntMatrix = {
    var i = 0
    var j = 0
		var ii = 0
		var jj = 0

    if(is_row) { // when input is a row vector
      val out = IntMatrix(m, n*length)
      ii = 0
      while (ii < m) {
        jj = 0
        while (jj < n) {
          j = 0
          while (j < length) {
            out(ii,jj*length+j) = this(j)
            j += 1
          }
          jj += 1
        }
        ii += 1
      }
      out
    }
    else {  // when input is a column vector
      val out = IntMatrix(m*length, n)
      ii = 0
      while (ii < m) {
        jj = 0
        while (jj < n) {
          i = 0
          while (i < length) {
            out(ii*length+i, jj) = this(i)
            i += 1
          }
          jj += 1
        }
        ii += 1
      }
      out
    }
  }

  private def nop = throw new RuntimeException("operation not implemented yet")

  def append (d: Int) {
    //nop
    ensureExtraCapacity(1)
    elements(length) = d
    length += 1
  }

  def apply(i: Int): Int = {
    //nop
    chkBounds(i)
    elements(offset+stride*i)
  }

  def update(i: Int, d: Int) {
    chkBounds(i)
    elements(offset+i*stride) = d
  }

 private[collection] def ensureExtraCapacity(extra: Int) {
    if(elements.length - length < extra) {
      increaseCapacity(length + extra)
    }
  }

  private[collection] def increaseCapacity(cap: Int) {
    var n = 4 max (elements.length * 2)
    while (n < cap) n *= 2
    val d = new Array[Int](n)
    Array.copy(elements, 0, d, 0, elements.length)
    elements = d
  }

}

// Read only version of the IntVector

object ConstIntVector {

  def apply(elms: Array[Int], length: Int) = new ConstIntVector(elms, length)
  def apply(elms: Array[Int], offset: Int, stride: Int, length: Int) = new ConstIntVector(elms, true, offset, stride, length)

}

class ConstIntVector(elms: Array[Int], is_row: Boolean, offset: Int, stride: Int, length: Int) extends IntVector(elms, is_row, offset, stride, length) {
  def this(elms: Array[Int], length: Int) = this(elms, true, 0, 1, length)

  override def update(i: Int, d: Int) {
    throw new UnsupportedOperationException("Cannot modify a ConstIntVector")
  }

  override def clone: IntVector = {
    val _elms = new Array[Int](length)
    var i = 0
    while(i != length){
      _elms(i)=this(i)
      i+=1
    }
    IntVector(_elms, is_row, length)
  }

}