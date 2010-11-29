package ppl.delite.core

/* A collection of types and utils every Delite app should include.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Jul 29, 2009
 * modified: Jul 29, 2009
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import ppl.delite.dsl.primitive._
import ppl.delite.dsl.optiml._
import specialized.DoubleMatrix

object appinclude {

  /**
   * Machinery for DeliteUnit
   */

  implicit def unitToDeliteUnit(u: Unit) = new DeliteUnit
  implicit def anyRefToDeliteUnit(a: AnyRef) = new DeliteUnit

  /**
   * Machinery for Delite proxy factories
   */
  implicit val deliteUnitProxyFactory = new DeliteProxyFactory[DeliteUnit] {
      def newProxy = new DeliteUnit
    }

  implicit def vecBuilder : Vector.Builder = new Vector.Builder
  implicit def vecProxyFactory[A : ClassManifest]: Vector.ProxyFactory[A] = new Vector.ProxyFactory[A]
  //implicit def vecVecProxyFactory[A : ClassManifest]: Vector.ProxyFactory[Vector[A]] = new Vector.ProxyFactory[Vector[A]]{}
  //implicit val vecVec_ProxyFactory: Vector.ProxyFactory[Vector[_]] = vecProxyFactory[Vector[_]]
  implicit val doubleVecProxyFactory: Vector.ProxyFactory[Double] = vecProxyFactory[Double]
  implicit val floatVecProxyFactory: Vector.ProxyFactory[Float] = vecProxyFactory[Float]
  implicit val intVecProxyFactory: Vector.ProxyFactory[Int] = vecProxyFactory[Int]
  implicit val longVecProxyFactory: Vector.ProxyFactory[Long] = vecProxyFactory[Long]
  implicit val byteVecProxyFactory: Vector.ProxyFactory[Byte] = vecProxyFactory[Byte]
  implicit val booleanVecProxyFactory: Vector.ProxyFactory[Boolean] = vecProxyFactory[Boolean]

  def vecViewProxyFactory[A : ClassManifest]: Vector.ViewProxyFactory[A] = new Vector.ViewProxyFactory[A]
  val doubleVecViewProxyFactory: Vector.ViewProxyFactory[Double] = vecViewProxyFactory[Double]
  val floatVecViewProxyFactory: Vector.ViewProxyFactory[Float] = vecViewProxyFactory[Float]
  val intVecViewProxyFactory: Vector.ViewProxyFactory[Int] = vecViewProxyFactory[Int]
  val longVecViewProxyFactory: Vector.ViewProxyFactory[Long] = vecViewProxyFactory[Long]
  val booleanVecViewProxyFactory: Vector.ViewProxyFactory[Boolean] = vecViewProxyFactory[Boolean]


  //implicit def bestEffortVecProxyFactory[A : ClassManifest]: BestEffortVector.ProxyFactory[A] = new BestEffortVector.ProxyFactory[A]{}
  //implicit val bestEffortDblVecProxyFactory: BestEffortVector.ProxyFactory[Double] = bestEffortVecProxyFactory[Double]
  //implicit val bestEffortIntVecProxyFactory: BestEffortVector.ProxyFactory[Int] = bestEffortVecProxyFactory[Int]

  implicit def matBuilder : Matrix.Builder = new Matrix.Builder
  implicit def matProxyFactory[A : ClassManifest]: Matrix.ProxyFactory[A] = new Matrix.ProxyFactory[A]
  implicit val doubleMatFactory: Matrix.ProxyFactory[Double] = matProxyFactory[Double]
  implicit val intMatFactory: Matrix.ProxyFactory[Int] = matProxyFactory[Int]
  implicit val longMatFactory: Matrix.ProxyFactory[Long] = matProxyFactory[Long]
  implicit val floatMatFactory: Matrix.ProxyFactory[Float] = matProxyFactory[Float]
  implicit val byteMatFactory: Matrix.ProxyFactory[Byte] = matProxyFactory[Byte]
  implicit val booleanMatFactory: Matrix.ProxyFactory[Boolean] = matProxyFactory[Boolean]

  implicit def SMatProxyFactory[A : ClassManifest]: SparseMatrix.ProxyFactory[A] = new SparseMatrix.ProxyFactory[A]
  implicit val doubleSMatFactory: SparseMatrix.ProxyFactory[Double] = SMatProxyFactory[Double]  

  /* arithmetic */
  implicit val doubleArithOps : ArithOps[Double] = new ArithOps[Double] {
    def +=(a: Double, b: Double) = a+b
    def +(a: Double, b: Double) = a+b
    def -(a: Double, b: Double) = a-b
    def *(a: Double, b: Double) = a*b
    def /(a: Double, b: Double) = a/b
    def zero = 0
    def unary_-(a: Double) = -a
    def abs(a: Double) = Math.abs(a)
    def exp(a: Double) = Math.exp(a)
    def >(a: Double, b: Double) = if (a > b) 1 else 0
    def <(a: Double, b: Double) = if (a < b) 1 else 0
  }
  implicit val intArithOps : ArithOps[Int] = new ArithOps[Int] {
    def +=(a: Int, b: Int) = a+b
    def +(a: Int, b: Int) = a+b
    def -(a: Int, b: Int) = a-b
    def *(a: Int, b: Int) = a*b
    def /(a: Int, b: Int) = a/b
    def zero = 0
    def unary_-(a: Int) = -a
    def abs(a: Int) = Math.abs(a)
    def exp(a: Int) = math.exp(a).asInstanceOf[Int]
    def >(a: Int, b: Int) = if (a > b) 1 else 0
    def <(a: Int, b: Int) = if (a < b) 1 else 0
  }
  implicit val longArithOps : ArithOps[Long] = new ArithOps[Long] {
    def +=(a: Long, b: Long) = a+b
    def +(a: Long, b: Long) = a+b
    def -(a: Long, b: Long) = a-b
    def *(a: Long, b: Long) = a*b
    def /(a: Long, b: Long) = a/b
    def zero = 0
    def unary_-(a: Long) = -a
    def abs(a: Long) = Math.abs(a)
    def exp(a: Long) = Math.exp(a).asInstanceOf[Long]
    def >(a: Long, b: Long) = if (a > b) 1 else 0
    def <(a: Long, b: Long) = if (a < b) 1 else 0
  }
  implicit val floatArithOps : ArithOps[Float] = new ArithOps[Float] {
    def +=(a: Float, b: Float) = a+b
    def +(a: Float, b: Float) = a+b
    def -(a: Float, b: Float) = a-b
    def *(a: Float, b: Float) = a*b
    def /(a: Float, b: Float) = a/b
    def zero = 0
    def unary_-(a: Float) = -a
    def abs(a: Float) = Math.abs(a)
    def exp(a: Float) = Math.exp(a).asInstanceOf[Float]
    def >(a: Float, b: Float) = if (a > b) 1 else 0
    def <(a: Float, b: Float) = if (a < b) 1 else 0
  }
  implicit val booleanArithOps : ArithOps[Boolean] = new ArithOps[Boolean] {
    def +=(a: Boolean, b: Boolean) = throw new UnsupportedOperationException()
    def +(a: Boolean, b: Boolean) = a | b
    def -(a: Boolean, b: Boolean) = throw new UnsupportedOperationException()
    def *(a: Boolean, b: Boolean) = a & b
    def /(a: Boolean, b: Boolean) = throw new UnsupportedOperationException()
    def zero = false
    def unary_-(a: Boolean) = throw new UnsupportedOperationException()
    def abs(a: Boolean) = a
    def exp(a: Boolean) = throw new UnsupportedOperationException()
    def >(a: Boolean, b: Boolean) = throw new UnsupportedOperationException()
    def <(a: Boolean, b: Boolean) = throw new UnsupportedOperationException()
  }

  implicit def vectorArithOps[T](implicit ops: ArithOps[T], c: ClassManifest[T]) : ArithOps[Vector[T]] = new ArithOps[Vector[T]] {
    def +=(a: Vector[T], b: Vector[T]) = a.+=(b)
    def +(a: Vector[T], b: Vector[T]) = a + b
    def -(a: Vector[T], b: Vector[T]) = a - b
    def *(a: Vector[T], b: Vector[T]) = a * b
    def /(a: Vector[T], b: Vector[T]) = a / b
    def zero = throw new UnsupportedOperationException() //TODO: figure out the size
    def unary_-(a: Vector[T]) = throw new UnsupportedOperationException()
    def abs(a: Vector[T]) = a.abs
    def exp(a: Vector[T]) = throw new UnsupportedOperationException()
    def >(a: Vector[T], b: Vector[T]) = throw new UnsupportedOperationException()
    def <(a: Vector[T], b: Vector[T]) = throw new UnsupportedOperationException()
  }

  implicit val doubleMatArithOps = matrixArithOps[Double]
  
  implicit def matrixArithOps[T](implicit ops: ArithOps[T], c: ClassManifest[T]) : ArithOps[Matrix[T]] = new ArithOps[Matrix[T]] {
    def +=(a: Matrix[T], b: Matrix[T]) = a.+=(b)
    def +(a: Matrix[T], b: Matrix[T]) = a + b
    def -(a: Matrix[T], b: Matrix[T]) = a - b
    def *(a: Matrix[T], b: Matrix[T]) = a dot b
    def /(a: Matrix[T], b: Matrix[T]) = a / b
    def zero = throw new UnsupportedOperationException() //TODO: figure out the size
    def unary_-(a: Matrix[T]) = -a
    def abs(a: Matrix[T]) = a.abs
    def exp(a: Matrix[T]) = a.exp
    def >(a: Matrix[T], b: Matrix[T]) = a > b
    def <(a: Matrix[T], b: Matrix[T]) = a < b
  }

  /**
   * Machinery for Delite primitive conversions.
   */
  
    //implicit def DelitePrimitiveToPrimitive[T](a: DelitePrimitive[T]) = a.value

    implicit def DoubleToDeliteDouble(a: Double) = DeliteDouble(a)
    implicit def FloatToDeliteFloat(a: Float) = DeliteFloat(a)
    implicit def FloatToDeliteDouble(a: Float) = DeliteDouble(a.toDouble)
    implicit def IntToDeliteInt(a: Int) = DeliteInt(a)
    implicit def IntToDeliteLong(a: Int) = DeliteLong(a.toLong)
    implicit def IntToDeliteFloat(a: Int) = DeliteFloat(a.toFloat)
    implicit def IntToDeliteDouble(a: Int) = DeliteDouble(a.toDouble)
    implicit def LongToDeliteLong(a: Long) = DeliteLong(a)
    implicit def LongToDeliteFloat(a: Long) = DeliteFloat(a.toFloat)
    implicit def LongToDeliteDouble(a: Long) = DeliteDouble(a.toDouble)
    implicit def ShortToDeliteShort(a: Short) = DeliteShort(a)
    implicit def ByteToDeliteByte(a: Byte) = DeliteByte(a)
    implicit def CharToDeliteChar(a: Char) = DeliteChar(a)
    implicit def BooleanToDeliteBoolean(a: Boolean) = DeliteBoolean(a)

    // these will force -- should be used with care
    implicit def DeliteDoubleToDouble(a: DeliteDouble) = a.value
    implicit def DeliteFloatToFloat(a: DeliteFloat) = a.value
    implicit def DeliteFloatToDouble(a: DeliteFloat) = a.value.toDouble
    implicit def DeliteIntToInt(a: DeliteInt) = a.value
    implicit def DeliteIntToFloat(a: DeliteInt) = a.value.toFloat
    implicit def DeliteIntToDouble(a: DeliteInt) = a.value.toDouble
    implicit def DeliteIntToLong(a: DeliteInt) = a.value.toLong
    implicit def DeliteLongToLong(a: DeliteLong) = a.value
    implicit def DeliteLongToFloat(a: DeliteLong) = a.value.toFloat
    implicit def DeliteLongToDouble(a: DeliteLong) = a.value.toDouble
    implicit def DeliteShortToShort(a: DeliteShort) = a.value
    implicit def DeliteByteToByte(a: DeliteByte) = a.value
    implicit def DeliteCharToChar(a: DeliteChar) = a.value
    implicit def DeliteBooleanToBoolean(a: DeliteBoolean) = a.value

    // proxies
    implicit val deliteDoubleProxyFactory = DeliteDouble.proxyFactory
    implicit val deliteFloatProxyFactory = DeliteFloat.proxyFactory
    implicit val deliteIntProxyFactory = DeliteInt.proxyFactory
    implicit val deliteShortProxyFactory = DeliteShort.proxyFactory
    implicit val deliteLongProxyFactory = DeliteLong.proxyFactory
    implicit val deliteCharProxyFactory = DeliteChar.proxyFactory
    implicit val deliteByteProxyFactory = DeliteByte.proxyFactory
    implicit val deliteBooleanProxyFactory = DeliteBoolean.proxyFactory

    /**
     * Machinery for Delite collections.
     */

    // proxies
    implicit val deliteIntVecProxyFactory: Vector.ProxyFactory[DeliteInt] = vecProxyFactory[DeliteInt]
    implicit val deliteIntMatProxyFactory: Matrix.ProxyFactory[DeliteInt] = matProxyFactory[DeliteInt]
    implicit val deliteShortVecProxyFactory: Vector.ProxyFactory[DeliteShort] = vecProxyFactory[DeliteShort]
    implicit val deliteShortMatProxyFactory: Matrix.ProxyFactory[DeliteShort] = matProxyFactory[DeliteShort]
    implicit val deliteLongVecProxyFactory: Vector.ProxyFactory[DeliteLong] = vecProxyFactory[DeliteLong]
    implicit val deliteLongMatProxyFactory: Matrix.ProxyFactory[DeliteLong] = matProxyFactory[DeliteLong]
    implicit val deliteDoubleVecProxyFactory: Vector.ProxyFactory[DeliteDouble] = vecProxyFactory[DeliteDouble]
    implicit val deliteDoubleMatProxyFactory: Matrix.ProxyFactory[DeliteDouble] = matProxyFactory[DeliteDouble]
    implicit val deliteFloatVecProxyFactory: Vector.ProxyFactory[DeliteFloat] = vecProxyFactory[DeliteFloat]
    implicit val deliteFloatMatProxyFactory: Matrix.ProxyFactory[DeliteFloat] = matProxyFactory[DeliteFloat]
    implicit val deliteCharVecProxyFactory: Vector.ProxyFactory[DeliteChar] = vecProxyFactory[DeliteChar]
    implicit val deliteCharMatProxyFactory: Matrix.ProxyFactory[DeliteChar] = matProxyFactory[DeliteChar]
    implicit val deliteByteVecProxyFactory: Vector.ProxyFactory[DeliteByte] = vecProxyFactory[DeliteByte]
    implicit val deliteByteMatProxyFactory: Matrix.ProxyFactory[DeliteByte] = matProxyFactory[DeliteByte]

    /* arithmetic for our sad primitive types */
    implicit val deliteDoubleArithOps : ArithOps[DeliteDouble] = new ArithOps[DeliteDouble] {
      def +=(a: DeliteDouble, b: DeliteDouble) = a+b
      def +(a: DeliteDouble, b: DeliteDouble) = a+b
      def -(a: DeliteDouble, b: DeliteDouble) = a-b
      def *(a: DeliteDouble, b: DeliteDouble) = a*b
      def /(a: DeliteDouble, b: DeliteDouble) = a/b
      def zero = DoubleToDeliteDouble(0)
      def unary_-(a: DeliteDouble) = -a
      def abs(a: DeliteDouble) = a.abs
      def exp(a: DeliteDouble) = a.exp
      def >(a: DeliteDouble, b: DeliteDouble) = if (a > b) 1 else 0
      def <(a: DeliteDouble, b: DeliteDouble) = if (a < b) 1 else 0
    }
    implicit val deliteIntArithOps : ArithOps[DeliteInt] = new ArithOps[DeliteInt] {
      def +=(a: DeliteInt, b: DeliteInt) = a+b
      def +(a: DeliteInt, b: DeliteInt) = a+b
      def -(a: DeliteInt, b: DeliteInt) = a-b
      def *(a: DeliteInt, b: DeliteInt) = a*b
      def /(a: DeliteInt, b: DeliteInt) = a/b
      def zero = IntToDeliteInt(0)
      def unary_-(a: DeliteInt) = -1*a
      def abs(a: DeliteInt) = a.abs
      def exp(a: DeliteInt) = a.exp
      def >(a: DeliteInt, b: DeliteInt) = if (a > b) 1 else 0
      def <(a: DeliteInt, b: DeliteInt) = if (a < b) 1 else 0
    }
    implicit val deliteLongArithOps : ArithOps[DeliteLong] = new ArithOps[DeliteLong] {
      def +=(a: DeliteLong, b: DeliteLong) = a+b
      def +(a: DeliteLong, b: DeliteLong) = a+b
      def -(a: DeliteLong, b: DeliteLong) = a-b
      def *(a: DeliteLong, b: DeliteLong) = a*b
      def /(a: DeliteLong, b: DeliteLong) = a/b
      def zero = LongToDeliteLong(0)
      def unary_-(a: DeliteLong) = -1*a
      def abs(a: DeliteLong) = a.abs
      def exp(a: DeliteLong) = a.exp
      def >(a: DeliteLong, b: DeliteLong) = if (a > b) 1 else 0
      def <(a: DeliteLong, b: DeliteLong) = if (a < b) 1 else 0
    }
    implicit val deliteFloatArithOps : ArithOps[DeliteFloat] = new ArithOps[DeliteFloat] {
      def +=(a: DeliteFloat, b: DeliteFloat) = a+b
      def +(a: DeliteFloat, b: DeliteFloat) = a+b
      def -(a: DeliteFloat, b: DeliteFloat) = a-b
      def *(a: DeliteFloat, b: DeliteFloat) = a*b
      def /(a: DeliteFloat, b: DeliteFloat) = a/b
      def zero = FloatToDeliteFloat(0)            
      def unary_-(a: DeliteFloat) = -1*a
      def abs(a: DeliteFloat) = a.abs
      def exp(a: DeliteFloat) = a.exp
      def >(a: DeliteFloat, b: DeliteFloat) = if (a > b) 1 else 0
      def <(a: DeliteFloat, b: DeliteFloat) = if (a < b) 1 else 0
    }


  /**
   * Machinery for primitive class manifests.
   */

  val DELITE_DOUBLE_MANIFEST = classManifest[DeliteDouble]
  val DELITE_FLOAT_MANIFEST = classManifest[DeliteFloat]
  val DELITE_INT_MANIFEST = classManifest[DeliteInt]
  val DELITE_LONG_MANIFEST = classManifest[DeliteLong]
  val DELITE_BOOLEAN_MANIFEST = classManifest[DeliteBoolean]

}

