package ppl.apps.ml.baseline.collection

import ppl.delite.core.Config
import ppl.delite.cnative.DeliteNative


object DoubleMatrix {

  
  val randRef = new scala.util.Random(100)
  
  /**
   * constructors
   */
  def apply(h: Int, w: Int): DoubleMatrix = {
    val elms =  new Array[Double](h*w)
    new DoubleMatrix(elms,h,w)
  }

  def apply(elms: Array[Double], h: Int, w:Int) = new DoubleMatrix(elms,h,w)

  def apply(d: Double, v: DoubleVector): DoubleMatrix = {
    val mat = DoubleMatrix(v.length, 2)
    var i = 0
    while (i != v.length) {
      mat.elements(i*mat.width) = d
      mat.elements(i*mat.width + 1) = v.elements(v.offset + i*v.stride)
      i += 1
    }
    mat
  }

  def random(h: Int, w: Int, mult: Double = 1.0) = {
    val size = h*w
    val elms = new Array[Double](size)
    var idx = 0
    while(idx != size){
      elms(idx) = randRef.nextDouble * mult      
      idx += 1
    }
    DoubleMatrix(elms, h, w)
  }

  def randomGaussian(h: Int, w: Int, mult: Double = 1.0) = {
    val size = h*w
    val elms = new Array[Double](size)
    var idx = 0
    while(idx != size){
      elms(idx) = randRef.nextGaussian.floatValue * mult      
      idx += 1
    }
    DoubleMatrix(elms, h, w)
  }

  def identity(n: Int): DoubleMatrix = {
    val elms = new Array[Double](n*n)
    var i = 0
    while (i != n) {
      elms(i*n + i) = 1.0 //insert 1 on diagonal
      i += 1
    }
    DoubleMatrix(elms, n, n)
  }

  def diag (w: Int, vals: DoubleVector): DoubleMatrix = {
    if (w != vals.length) throw new Exception("diag: dimensions don't agree")
    val elms = new Array[Double](w*w)
    var i = 0
    while (i < w) {
      elms(i*w + i) = vals(i)
      i += 1
    }
    DoubleMatrix(elms, w, w)
  }

}

class DoubleMatrix(elms: Array[Double], h: Int, w: Int) extends Matrix {

 

  private[collection] var elements = elms
  val width: Int = w
  var size = h*w

  def pprint {
    val str = new StringBuilder(super.toString)
    str.append("\n[ ")
    var idx = 0
    while(idx != size) {
      str.append(elements(idx).toString + " ")
      idx +=1
      if(idx % width == 0 && idx != size)
        str.append("\n  ")
    }
    str.append("]")
    str.toString
    println(str)
  }

  def selectRandomRows(n: Int) = {
    val s_mat = DoubleMatrix(n,width)

    var idx = 0
    while( idx != n) {
      s_mat(idx) = this((DoubleMatrix.randRef.nextDouble*height).asInstanceOf[Int])
      idx += 1
    }
    s_mat
  }

  /**
   * This is a mutating OP that mutates the matrix m by append to it a double vector v
   */
  def ::=(v: DoubleVector) {
    super.chkBounds(this, v)
    ensureExtraCapacity(v.length)
    var idx = 0
    var sizet = this.size
    val end = v.length
    val voff = v.offset
    val vstr = v.stride
    while(idx != end) {
      this.elements(sizet) = v.elements(voff + idx*vstr)
      idx += 1
      sizet += 1
    }
    this.size = sizet
  }

  def apply(i: Int) = {
    chkBounds(i)
    ConstDoubleVector(elements, i*width, 1, width)    
  }

  def apply(i: Int, j: Int) = {
    elements(i*width+j)
  }

  /**
   * Matrix Addition
   */
  def +(m: DoubleMatrix) = {
    super.chkBounds(this, m)
    val elms = new Array[Double](height * width)
    var idx = 0
    val end = size
    while (idx != end) {
      elms(idx) = this.elements(idx) + m.elements(idx)
      idx += 1
    }
    DoubleMatrix(elms, height, width)
  }

  def +(d: Double) = {
    val elms = new Array[Double](height * width)
    var i = 0
    while (i != size) {
      elms(i) = this.elements(i) + d
      i += 1
    }
    DoubleMatrix(elms, height, width)
  }

/**
   * Matrix substraction
   */
  def -(m: DoubleMatrix) = {
    super.chkBounds(this, m)
    val elms = new Array[Double](height * width)
    var idx = 0
    val end = size
    while (idx != end) {
      elms(idx) = this.elements(idx) - m.elements(idx)
      idx += 1
    }
    DoubleMatrix(elms, height, width)
  }

  def -(d: Double) = {
    val elms = new Array[Double](height * width)
    var i = 0
    while (i != size) {
      elms(i) = this.elements(i) - d
      i += 1
    }
    DoubleMatrix(elms, height, width)
  }


  def /(d: Double) = {
    val elms = new Array[Double](height * width)
    var i = 0
    while (i != size) {
      elms(i) = this.elements(i) / d
      i += 1
    }
    DoubleMatrix(elms, height, width)
  }

  def unary_- : DoubleMatrix = {
    val elms = new Array[Double](height * width)
    var i = 0
    while(i != size) {
      elms(i) = -this.elements(i)
      i+=1
    }
    DoubleMatrix(elms, height, width)      
  }


  /**
   * Mutating Matrix Addition
   */
  def +=(m: DoubleMatrix) {
    super.chkBounds(this,m)
    var idx = 0
    val end = size
    while (idx != end) {
      this.elements(idx) = this.elements(idx) + m.elements(idx)
      idx += 1
    }
  }

  /**
   *  Matrix Multiplication
   */
  def *(m: DoubleMatrix) = {
    if(width != m.height) throw new IllegalArgumentException("Matrix inner dimensions must agree") //inner dimensions must agree
    val elms = new Array[Double](height * m.width) //new size will be outer dimensions

    if(Config.useNativeLibs) {
      DeliteNative.matrixMultDouble(this.elements, m.elements, elms,height,width, m.width)
    } else {
      val heightl = height
      val widthr = m.width
      val width1 = width
      val widthm = widthr
      var c = 0
      while (c != width1) {
        var rl = 0
        while (rl != heightl) {
          var cr = 0
          while (cr != widthr) {
            elms(rl*widthm + cr) += (this.elements(rl*width1 + c) * m.elements(c*widthr + cr))
            cr += 1
          }
        rl += 1
        }
        c+=1
      }
    }

    DoubleMatrix(elms, height, m.width)
  }

  /**
   * Scalar Multiplication
   */
  def *(d: Double) = {
    val elms = new Array[Double](height * width)
    var idx = 0
    val end = size
    while (idx != end) {
      elms(idx) = this.elements(idx) * d
      idx += 1
    }
    DoubleMatrix(elms, height, width)
  }

  /**
   * Matrix-Vector Multiplication
   */
  def *(v: DoubleVector) = {
    if(width != v.length)
      throw new IllegalArgumentException("Inner dimensions must agree") //inner dimensions must agree
    val vec = new Array[Double](height)
    if(Config.useNativeLibs) {
      DeliteNative.matVMultDouble(this.elements,v.elements,vec,height, width,v.offset,v.stride)
    } else {

      val voff = v.offset
      val vstr = v.stride
      var rl = 0
      while (rl != height) {
        var c = 0
        var acc = 0.0
        while (c != width) {
          acc += (this.elements(rl*width + c) * v.elements(voff + c*vstr))
          c += 1
        }
        vec(rl) = acc
        rl += 1
      }
    }
    DoubleVector(vec, height)
  }

  /**
   *   Matrix-Vector Dot
   */

  def dot(v: DoubleVector) = {
    val elms = new Array[Double](height * width)
    val voff = v.offset
    val vstr = v.stride
    if (v.isRow) {
      if (width != v.length) throw new IllegalArgumentException("Inner dimensions must agree")
      var r = 0
      while (r != height) {
        var c = 0
        while (c != width) {
          elms(r*width + c) = this.elements(r*width + c) * v.elements(voff + c*vstr)
          c += 1
        }
        r += 1
      }
    } else {
      if (height != v.length) throw new IllegalArgumentException
      var r = 0
      while (r != height) {
        var c = 0
        while (c != width) {
          elms(r*width + c) = this.elements(r*width + c) * v.elements(voff + r*vstr)
          c += 1
        }
        r += 1
      }
    }
    DoubleMatrix(elms, height, width)
  }


  def dot(m: DoubleMatrix) = {
    val elms = new Array[Double](size)
    var i = 0
    while (i != size) {
      elms(i) = this.elements(i) * m.elements(i)
      i += 1
    }
    DoubleMatrix(elms, height, width)
  }

  /**
   * Inverse
   */

  def inv = {
    if (height != width) throw new IllegalArgumentException //matrix must be square
    //augment the matrix with the identity matrix of the same size
    val id = DoubleMatrix.identity(width)
    val augMat = insertAllCols(this, id)
    //perform row reductions
    val redMat = rreduce(augMat)
    //strip off augment matrix
    val ret = removeCols(redMat, 0, width)
    ret
  }

  def abs: DoubleMatrix = {
    var _elms = new Array[Double](size)
    var idx = 0
    while(idx != size) {
      _elms(idx) = Math.abs(elements(idx))
      idx += 1
    }
    DoubleMatrix(_elms, height, width)
  }

  def > (m: DoubleMatrix): DoubleMatrix = {
    var _elms = new Array[Double](size)
    var i = 0
    while(i != size) {
      _elms(i) = if(elements(i) > m.elements(i)) 1 else 0
      i += 1
    }
    DoubleMatrix(_elms, height, width)
  }

  def sum = {
    var _sum = 0.0
    var idx = 0
    while(idx != size) {
      _sum += elements(idx)      
      idx += 1
    }
    _sum
  }

  def insertAllCols(m1: DoubleMatrix, m2: DoubleMatrix) = {
    val height = m1.height
    val w1 = m1.width
    val w2 = m2.width
    val wr = w1 + w2
    val elms = new Array[Double](height * wr)
    var r = 0
    while (r != height) {
      var c1 = 0
      while (c1 != w1) {
        elms(r*wr + c1) = m1.elements(r*w1 + c1)
        c1 += 1
      }
      var c2 = 0
      while (c2 != w2) {
        elms(r*wr + w1 + c2) = m2.elements(r*w2 + c2)
        c2 += 1
      }
      r += 1
    }
    DoubleMatrix(elms, height, wr)
  }

  def rreduce(m: DoubleMatrix) = {
    val currentMat = m.clone
    val width = currentMat.width
    val height = currentMat.height
    var lead = 0
    var r = 0
    while (r != height) {
      var i = r
      while (currentMat.elements(i*width + lead) == 0.0) {
        i += 1
        if (width == i) {
          i = r
          lead += 1
        }
      }

      val tmpRow = currentMat(i)
      currentMat(i) = currentMat(r)
      currentMat(r) = tmpRow

      currentMat(r) = currentMat(r) / currentMat.elements(r*width + lead)

      var j = 0
      while (j != height) {
        if (j != r)
          currentMat(j) = currentMat(j) - currentMat(r)*currentMat.elements(j*width + lead)
        j += 1
      }
      lead += 1
      r += 1
    }
    currentMat
  }

  def removeCols(m: DoubleMatrix, start: Int, end: Int) = {
    val height = m.height
    val width = m.width
    val wr = end-start
    val elms = new Array[Double](height * wr)
    var r = 0
    while (r != height) {
      var c = 0
      var idx = 0
      while (c != width) {
        if ((c < start) || (c >= end)) {
          elms(r*wr + idx) = m.elements(r*width + c)
          idx += 1
        }
        c += 1
      }
      r += 1
    }
    DoubleMatrix(elms, height, wr)
  }

  /**
   * TRANSPOSE
   */
  def trans = {
    val elms = new Array[Double](width * height) //transposed
    var r = 0
    while (r != height) {
      var c = 0
      while (c != width) {
        elms(c*height + r) = elements(r*width + c)
        c += 1
      }
      r += 1
    }
    DoubleMatrix(elms, width, height)
  }

  /**
   * Slice Matrix
   */
  def sliceRows(begin: Int, end: Int): DoubleMatrix =  {
    chkRange(begin, end)
    val out = DoubleMatrix(end-begin, width)
    var i = begin
    while(i != end){
      out(i-begin) = this(i).clone
      i += 1
    }
    out
  }

  /**
   *  CLONE DOUBLEMATRIX
   */
  override def clone = {
    val elms = new Array[Double](height * width)
    var r = 0
    while (r != height) {
      var c = 0
      while (c != width) {
        elms(r*width + c) = this.elements(r*width + c)
        c += 1
      }
      r += 1
    }
    DoubleMatrix(elms, height, width)
  }

  /**
   * Sum Columns
   */
  def sumColumns: DoubleVector = {
    val elms = new Array[Double](width)
    var i=0
    while(i!=height){
      var j=0
      while(j!=width){
        elms(j) += this(i,j)
        j+=1
      }
      i+=1
    }
    DoubleVector(elms, width)
  }

  def replicateMatrix(m: Int, n: Int): DoubleMatrix = {
    val out = DoubleMatrix(m*height, n*height)
    //var index = 0
    var i = 0
    var j = 0
    var ii = 0
    var jj = 0
    while(ii != m) {
      i = 0
      while(i != height) {
        jj = 0
        while(jj != n) {
          j = 0
          while(j != width) {
            out(ii*height+i, jj*width+j) = this(i,j)
            j += 1
          }
          jj += 1
        }
        i += 1
      }
      ii += 1
    }
    out
  }

  def reciprocal: DoubleMatrix = {
    val _elms = new Array[Double](size)
    var i = 0
    while(i != size) {
      _elms(i) = 1 / elements(i)
      i += 1
    }
    DoubleMatrix(_elms,height, width)
  }

  def exp: DoubleMatrix = {
    val _elms = new Array[Double](size)
    var i = 0
    while(i != size) {
      _elms(i) = Math.exp(elements(i))
      i += 1
    }
    DoubleMatrix(_elms,height, width)
  }

  /**
   * MAP TO VECTOR
   */
  def mapToVec(f: DoubleVector => Double) = {
    val elms = new Array[Double](height)
    var i = 0
    while (i != height) {
      elms(i) = f(this(i))
      i += 1
    }
    DoubleVector(elms, height)
  }

  def map(f: Double => Int): IntVector = nop



  def update(i:Int, j:Int, d: Double) {
    super.chkBounds(i,j)
    elements(i*width + j) = d
  }

  def update(i: Int, v: DoubleVector) = {
    if (v.length != width || !v.isRow) throw new IllegalArgumentException
    var j = 0
    val voff = v.offset
    val vstr = v.stride
    val end = v.length
    while (j != end) {
      this.elements(i*width + j) = v.elements(voff + j*vstr)
      j += 1
    }
  }

  private[collection] def ensureExtraCapacity(extra: Int) {
    if(elements.length - size < extra) {
      increaseCapacity(size + extra)
    }
  }

  private[collection] def increaseCapacity(cap: Int) {
    var n = 4 max (elements.length * 2)
    while (n < cap) n *= 2
    val d =  new Array[Double](n)
    elements.copyToArray(d)
    elements = d
  }

  private def nop = throw new RuntimeException("This operation is not yet supported");

}
