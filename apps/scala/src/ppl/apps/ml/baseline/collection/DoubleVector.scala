package ppl.apps.ml.baseline.collection

object DoubleVector {

  /**
   * Constructors
   */
  def apply(l: Int): DoubleVector = {
    val elms = new Array[Double](l)
    new DoubleVector(elms, l)
  }

  def apply(elms: Array[Double], length: Int) = new DoubleVector(elms, length)
  def apply(elms: Array[Double], is_row: Boolean, length: Int) = new DoubleVector(elms, is_row, length)
  def apply(elms: Array[Double], offset: Int, stride: Int, length: Int) = new DoubleVector(elms, offset, stride, length)
  def apply(elms: Array[Double], is_row: Boolean, offset: Int, stride: Int, length: Int) = new DoubleVector(elms, is_row, offset, stride, length)


  def uniform(start: Double, step_size: Double, end: Double): DoubleVector = {
    if ((step_size <= 0) || (end < start)) throw new IllegalArgumentException
    val length = Math.ceil((end-start)/step_size).asInstanceOf[Int]
    val vec = new Array[Double](length)
    var i = 0
    var cur = start
    while (i != length) {
      vec(i) = cur
      cur += step_size
      i += 1
    }
    DoubleVector(vec, length)
  }

  def range(start: Int, end: Int) = uniform(start, 1, end)

}

class DoubleVector(elms: Array[Double], is_row: Boolean, _offset: Int, _stride: Int, _length: Int   ) extends Vector {

  def this(elms: Array[Double], length: Int) = this(elms, true, 0 ,1 ,length)
  def this(elms: Array[Double], is_row: Boolean, length: Int) = this(elms, is_row, 0, 1, length)
  def this(elms: Array[Double], offset: Int, stride: Int, length: Int) = this(elms, true, offset, stride, length)

  private[collection] var elements: Array[Double] = elms
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

  def +(v: DoubleVector) = {
    chkBounds(this, v)
    val end = length
    val elms = new Array[Double](end)
    val v1off = offset
    val v1str = stride
    val v2off = v.offset
    val v2str = v.stride
    var idx = 0
    while(idx != end) {
      elms(idx) = this.elements(v1off + idx*v1str) + v.elements(v2off + idx*v2str)
      idx += 1
    }
    DoubleVector(elms, end)
  }

  /**
   * Vector Division by a single Double. All elements are divided by this double and a new DoubleVector is returned
   */
  def /(d: Double) = {
    val end = length
    val elms = new Array[Double](end)
    val voff = offset
    val vstr = stride
    var idx = 0
    while(idx != end) {
      elms(idx) = this.elements(voff + idx*vstr) / d
      idx += 1
    }
    DoubleVector(elms, end)
  }

  /**
   * Vector Subtraction
   */
  def -(v: DoubleVector) = {
    chkBounds(this, v)
    val end = length
    val elms = new Array[Double](end)
    val v1off = offset
    val v1str = stride
    val v2off = v.offset
    val v2str = v.stride
    var idx = 0
    while(idx != end) {
      elms(idx) = this.elements(v1off + idx*v1str) - v.elements(v2off + idx*v2str)
      idx += 1
    }
    DoubleVector(elms, end)
  }

  /**
   * Outer Product
   */
  def outer(v: DoubleVector) = {
    chkBounds(this, v)
    val lgt = length
    val elms = new Array[Double](lgt * lgt)
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
    DoubleMatrix(elms, lgt, lgt)
  }

  /**
   * Vector multiplication by a single double
   */
  def *(d: Double) = {
    val end = length
    val elms = new Array[Double](end)
    val voff = offset
    val vstr = stride
    var idx = 0
    while (idx != end) {
      elms(idx) = this.elements(voff + idx*vstr) * d
      idx += 1
    }
    DoubleVector(elms, end)
  }

  def *(d: DoubleVector) = {
    val end = length
    val elms = new Array[Double](end)
    val voff = offset
    val vstr = stride
    var idx = 0
    while (idx != end) {
      elms(idx) = this.elements(voff + idx*vstr) * d.elements(voff + idx*vstr)
      idx += 1
    }
    DoubleVector(elms, end)
  }

  def take(n: Int): DoubleVector = {
    val elms = new Array[Double](n)
    var i = 0
    val end = n-1
    while(i < end) {
      elms(i) = this.elements(i)
      i += 1
    }
    DoubleVector(elms, n)
  }

  /**
   * Inner Product
   */
  def dot(v: DoubleVector) = {
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
    val elms = new Array[Double](end)
    val voff = offset
    val vstr = stride
    var i = 0
    while (i != end) {
      elms(i) = this.elements(voff + i*vstr)
      i += 1
    }
    DoubleVector(elms, !isRow, end)
  }

  /**
   * Transposing Mutable Vector
   */
  def trans_m = {
    isRow = !isRow
  }

  def map(f: Double => Double) = {
    val end = length
    val elms = new Array[Double](end)
    val voff = offset
    val vstr = stride
    var idx = 0
    while (idx != end) {
      elms(idx) = f(this.elements(voff + idx*vstr))
      idx += 1
    }
    DoubleVector(elms, isRow, length)
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

  def replicateMatrix(m: Int, n: Int): DoubleMatrix = {
    var i = 0
    var j = 0
		var ii = 0
		var jj = 0

    if(is_row) { // when input is a row vector
      val out = DoubleMatrix(m, n*length)
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
      val out = DoubleMatrix(m*length, n)
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

  def append (d: Double) {
    //nop
    ensureExtraCapacity(1)
    elements(length) = d
    length += 1
  }

  def apply(i: Int): Double = {
    //nop
    chkBounds(i)
    elements(offset+stride*i)
  }

  def update(i: Int, d: Double) {
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
    val d = new Array[Double](n)
    Array.copy(elements, 0, d, 0, elements.length)
    elements = d
  }

  override def clone: DoubleVector = {
    val _elms = new Array[Double](length)
    var i = 0
    while(i != length){
      _elms(i)= this.elements(i)
      i+=1
    }
    DoubleVector(_elms, is_row, length)
  }

}

// Read only version of the DoubleVector

object ConstDoubleVector {

  def apply(elms: Array[Double], length: Int) = new ConstDoubleVector(elms, length)
  def apply(elms: Array[Double], offset: Int, stride: Int, length: Int) = new ConstDoubleVector(elms, true, offset, stride, length)

}

class ConstDoubleVector(elms: Array[Double], is_row: Boolean, offset: Int, stride: Int, length: Int) extends DoubleVector(elms, is_row, offset, stride, length) {
  def this(elms: Array[Double], length: Int) = this(elms, true, 0, 1, length)

  override def update(i: Int, d: Double) {
    throw new UnsupportedOperationException("Cannot modify a ConstDoubleVector")
  }

  override def clone: DoubleVector = {
    val _elms = new Array[Double](length)
    var i = 0
    while(i != length){
      _elms(i)=this(i)
      i+=1
    }
    DoubleVector(_elms, is_row, length)
  }

}
