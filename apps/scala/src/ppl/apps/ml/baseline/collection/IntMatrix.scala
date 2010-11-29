package ppl.apps.ml.baseline.collection

object IntMatrix {

  
  val randRef = new scala.util.Random(100)
  
  /**
   * constructors
   */
  def apply(h: Int, w: Int): IntMatrix = {
    val elms =  new Array[Int](h*w)
    new IntMatrix(elms,h,w)
  }

  def apply(elms: Array[Int], h: Int, w:Int) = new IntMatrix(elms,h,w)

  def apply(d: Int, v: IntVector): IntMatrix = {
    val mat = IntMatrix(v.length, 2)
    var i = 0
    while (i != v.length) {
      mat.elements(i*mat.width) = d
      mat.elements(i*mat.width + 1) = v.elements(v.offset + i*v.stride)
      i += 1
    }
    mat
  }

  def random(h: Int, w: Int, mult: Int = 1) = {
    val size = h*w
    val elms = new Array[Int](size)
    var idx = 0
    while(idx != size){
      elms(idx) = randRef.nextInt * mult      
      idx += 1
    }
    IntMatrix(elms, h, w)
  }

 

  def identity(n: Int): IntMatrix = {
    val elms = new Array[Int](n*n)
    var i = 0
    while (i != n) {
      elms(i*n + i) = 1 //insert 1 on diagonal
      i += 1
    }
    IntMatrix(elms, n, n)
  }

}

class IntMatrix(elms: Array[Int], h: Int, w: Int) extends Matrix {

 

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
    val s_mat = IntMatrix(n,width)

    var idx = 0
    while( idx != n) {
      s_mat(idx) = this((IntMatrix.randRef.nextInt*height).asInstanceOf[Int])
      idx += 1
    }
    s_mat
  }

  /**
   * This is a mutating OP that mutates the matrix m by append to it a Int vector v
   */
  def ::=(v: IntVector) {
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
    ConstIntVector(elements, i*width, 1, width)    
  }

  def apply(i: Int, j: Int) = {
    elements(i*width+j)
  }

  /**
   * Matrix Addition
   */
  def +(m: IntMatrix) = {
    super.chkBounds(this, m)
    val elms = new Array[Int](height * width)
    var idx = 0
    val end = size
    while (idx != end) {
      elms(idx) = this.elements(idx) + m.elements(idx)
      idx += 1
    }
    IntMatrix(elms, height, width)
  }

  def +(d: Int) = {
    val elms = new Array[Int](height * width)
    var i = 0
    while (i != size) {
      elms(i) = this.elements(i) + d
      i += 1
    }
    IntMatrix(elms, height, width)
  }

/**
   * Matrix substraction
   */
  def -(m: IntMatrix) = {
    super.chkBounds(this, m)
    val elms = new Array[Int](height * width)
    var idx = 0
    val end = size
    while (idx != end) {
      elms(idx) = this.elements(idx) - m.elements(idx)
      idx += 1
    }
    IntMatrix(elms, height, width)
  }

  def -(d: Int) = {
    val elms = new Array[Int](height * width)
    var i = 0
    while (i != size) {
      elms(i) = this.elements(i) - d
      i += 1
    }
    IntMatrix(elms, height, width)
  }


  def /(d: Int) = {
    val elms = new Array[Int](height * width)
    var i = 0
    while (i != size) {
      elms(i) = this.elements(i) / d
      i += 1
    }
    IntMatrix(elms, height, width)
  }

  def unary_- : IntMatrix = {
    val elms = new Array[Int](height * width)
    var i = 0
    while(i != size) {
      elms(i) = -this.elements(i)
      i+=1
    }
    IntMatrix(elms, height, width)      
  }


  /**
   * Mutating Matrix Addition
   */
  def +=(m: IntMatrix) {
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
  def *(m: IntMatrix) = {
    if(width != m.height) throw new IllegalArgumentException("Matrix inner dimensions must agree") //inner dimensions must agree
    val elms = new Array[Int](height * m.width) //new size will be outer dimensions
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
    IntMatrix(elms, height, m.width)
  }

  /**
   * Scalar Multiplication
   */
  def *(d: Int) = {
    val elms = new Array[Int](height * width)
    var idx = 0
    val end = size
    while (idx != end) {
      elms(idx) = this.elements(idx) * d
      idx += 1
    }
    IntMatrix(elms, height, width)
  }

  /**
   * Matrix-Vector Multiplication
   */
  def *(v: IntVector) = {
        if(width != v.length)
          throw new IllegalArgumentException("Inner dimensions must agree") //inner dimensions must agree
        val vec = new Array[Int](height)
        val voff = v.offset
        val vstr = v.stride
        var rl = 0
        while (rl != height) {
          var c = 0
          var acc = 0
          while (c != width) {
            acc += (this.elements(rl*width + c) * v.elements(voff + c*vstr))
            c += 1
          }
          vec(rl) = acc
          rl += 1
        }
        IntVector(vec, height)
      }

  /**
   *   Matrix-Vector Dot
   */

  def dot(v: IntVector) = {
    val elms = new Array[Int](height * width)
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
    IntMatrix(elms, height, width)
  }


  def dot(m: IntMatrix) = {
    val elms = new Array[Int](size)
    var i = 0
    while (i != size) {
      elms(i) = this.elements(i) * m.elements(i)
      i += 1
    }
    IntMatrix(elms, height, width)
  }

  /**
   * Inverse
   */

  def inv = {
    if (height != width) throw new IllegalArgumentException //matrix must be square
    //augment the matrix with the identity matrix of the same size
    val id = IntMatrix.identity(width)
    val augMat = insertAllCols(this, id)
    //perform row reductions
    val redMat = rreduce(augMat)
    //strip off augment matrix
    val ret = removeCols(redMat, 0, width)
    ret
  }

  def abs: IntMatrix = {
    var _elms = new Array[Int](size)
    var idx = 0
    while(idx != size) {
      _elms(idx) = Math.abs(elements(idx))
      idx += 1
    }
    IntMatrix(_elms, height, width)
  }

  def > (m: IntMatrix): IntMatrix = {
    var _elms = new Array[Int](size)
    var i = 0
    while(i != size) {
      _elms(i) = if(elements(i) > m.elements(i)) 1 else 0
      i += 1
    }
    IntMatrix(_elms, height, width)
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

  def insertAllCols(m1: IntMatrix, m2: IntMatrix) = {
    val height = m1.height
    val w1 = m1.width
    val w2 = m2.width
    val wr = w1 + w2
    val elms = new Array[Int](height * wr)
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
    IntMatrix(elms, height, wr)
  }

  def rreduce(m: IntMatrix) = {
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

  def removeCols(m: IntMatrix, start: Int, end: Int) = {
    val height = m.height
    val width = m.width
    val wr = end-start
    val elms = new Array[Int](height * wr)
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
    IntMatrix(elms, height, wr)
  }

  /**
   * TRANSPOSE
   */
  def trans = {
    val elms = new Array[Int](width * height) //transposed
    var r = 0
    while (r != height) {
      var c = 0
      while (c != width) {
        elms(c*height + r) = elements(r*width + c)
        c += 1
      }
      r += 1
    }
    IntMatrix(elms, width, height)
  }

  /**
   * Slice Matrix
   */
  def sliceRows(begin: Int, end: Int): IntMatrix =  {
    chkRange(begin, end)
    val out = IntMatrix(end-begin, width)
    var i = begin
    while(i != end){
      out(i-begin) = this(i).clone
      i += 1
    }
    out
  }

  /**
   *  CLONE IntMATRIX
   */
  override def clone = {
    val elms = new Array[Int](height * width)
    var r = 0
    while (r != height) {
      var c = 0
      while (c != width) {
        elms(r*width + c) = this.elements(r*width + c)
        c += 1
      }
      r += 1
    }
    IntMatrix(elms, height, width)
  }

  /**
   * Sum Columns
   */
  def sumColumns: IntVector = {
    val elms = new Array[Int](width)
    var i=0
    while(i!=height){
      var j=0
      while(j!=width){
        elms(j) += this(i,j)
        j+=1
      }
      i+=1
    }
    IntVector(elms, width)
  }

  def replicateMatrix(m: Int, n: Int): IntMatrix = {
    val out = IntMatrix(m*height, n*height)
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

  def reciprocal: IntMatrix = {
    val _elms = new Array[Int](size)
    var i = 0
    while(i != size) {
      _elms(i) = 1 / elements(i)
      i += 1
    }
    IntMatrix(_elms,height, width)
  }

  def exp: IntMatrix = {
    val _elms = new Array[Int](size)
    var i = 0
    while(i != size) {
      _elms(i) = Math.exp(elements(i)).intValue
      i += 1
    }
    IntMatrix(_elms,height, width)
  }

  /**
   * MAP TO VECTOR
   */
  def mapToVec(f: IntVector => Int) = {
    val elms = new Array[Int](height)
    var i = 0
    while (i != height) {
      elms(i) = f(this(i))
      i += 1
    }
    IntVector(elms, height)
  }

  def map(f: Int => Int): IntVector = nop



  def update(i:Int, j:Int, d: Int) {
    super.chkBounds(i,j)
    elements(i*width + j) = d
  }

  def update(i: Int, v: IntVector) = {
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
    val d =  new Array[Int](n)
    elements.copyToArray(d)
    elements = d
  }

  private def nop = throw new RuntimeException("This operation is not yet supported");

}