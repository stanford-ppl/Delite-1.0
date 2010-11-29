
/* Conversions for primitives to handle vector/matrix arithmetic
 * must be imported to be in scope!
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: 4/22/09
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.delite.dsl.optiml

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.primitive._


object Precursors {
  implicit def anyToAnyPrecursor[A,B](a: A)(implicit ops: ArithOps[B], conv: (A => B), c: ClassManifest[B]) = new Precursors.AnyToAnyPrecursor[A,B](a)

  // TODO: multiplying a DoubleVector by an IntVector requires an extra allocation (for the .toDouble call) in this
  //       implementation. we should be able to do point-wise operations by converting each element incrementally,
  //       instead of by allocating a new vector. e.g. def *[B](that: Vector[B])(implicit ops: ArithOps[T,B])
  implicit def intVecToDoubleVec(a: Vector[Int]) = a.toDouble
  implicit def intVecToFloatVec(a: Vector[Int]) = a.toFloat
  implicit def intVecToLongVec(a: Vector[Int]) = a.toLong
  implicit def longVecToDoubleVec(a: Vector[Long]) = a.toDouble
  implicit def longVecToFloatVec(a: Vector[Long]) = a.toFloat
  implicit def floatVecToDoubleVec(a: Vector[Float]) = a.toDouble
  
  implicit def intMatToDoubleMat(a: Matrix[Int]) = a.toDouble
  implicit def intMatToFloatMat(a: Matrix[Int]) = a.toFloat
  implicit def intMatToLongMat(a: Matrix[Int]) = a.toLong
  implicit def longMatToDoubleMat(a: Matrix[Long]) = a.toDouble
  implicit def longMatToFloatMat(a: Matrix[Long]) = a.toFloat
  implicit def floatMatToDoubleMat(a: Matrix[Float]) = a.toDouble
  
  class AnyToAnyPrecursor[A,B](a: A)(implicit ops: ArithOps[B], conv: (A => B), c: ClassManifest[B]) {
    def +(v: Vector[B]) = v+conv(a)
    def -(v: Vector[B]) = v-conv(a)
    def *(v: Vector[B]) = v*conv(a)
    def /(v: Vector[B]) = v/conv(a)
    
    def +(m: Matrix[B]) = m+conv(a)
    def -(m: Matrix[B]) = m-conv(a)
    def *(m: Matrix[B]) = m*conv(a)
    def /(m: Matrix[B]) = m/conv(a)
  }

  // for delite primitives, we still need a better solution
  implicit def intVecToDeliteDoubleVec(a: Vector[Int]) = a.map(e => DeliteDouble(e))
  implicit def intVecToDeliteFloatVec(a: Vector[Int]) = a.map(e => DeliteFloat(e))
  implicit def intVecToDeliteLongVec(a: Vector[Int]) = a.map(e => DeliteLong(e))
  implicit def longVecToDeliteDoubleVec(a: Vector[Long]) = a.map(e => DeliteDouble(e))
  implicit def longVecToDeliteFloatVec(a: Vector[Long]) = a.map(e => DeliteFloat(e))
  implicit def floatVecToDeliteDoubleVec(a: Vector[Float]) = a.map(e => DeliteDouble(e))
  
  implicit def deliteIntVecToIntVec(a: Vector[DeliteInt]) = a.map(e => e.value)
  implicit def deliteIntVecToDoubleVec(a: Vector[DeliteInt]) = a.map(e => e.value.toDouble)
  implicit def deliteIntVecToFloatVec(a: Vector[DeliteInt]) = a.map(e => e.value.toFloat)
  implicit def deliteIntVecToLongVec(a: Vector[DeliteInt]) = a.map(e => e.value.toLong)
  implicit def deliteLongVecToLongVec(a: Vector[DeliteLong]) = a.map(e => e.value)
  implicit def deliteLongVecToDoubleVec(a: Vector[DeliteLong]) = a.map(e => e.value.toDouble)
  implicit def deliteLongVecToFloatVec(a: Vector[DeliteLong]) = a.map(e => e.value.toFloat)
  implicit def deliteFloatVecToDoubleVec(a: Vector[DeliteFloat]) = a.map(e => e.value.toDouble)
  implicit def deliteFloatVecToFloatVec(a: Vector[DeliteFloat]) = a.map(e => e.value)
  implicit def deliteDoubleVecToDoubleVec(a: Vector[DeliteDouble]) = a.map(e => e.value)
  //implicit def deliteShortVecToShortVec(a: Vector[DeliteShort]) : Vector[Short] = a.map(e => e.value)(refVecFactory[Short])
  //implicit def deliteByteVecToByteVec(a: Vector[DeliteByte]) : Vector[Byte] = a.map(e => e.value)(refVecFactory[Byte])
  //implicit def deliteCharVecToCharVec(a: Vector[DeliteChar]) : Vector[Char] = a.map(e => e.value)(refVecFactory[Char])  


  implicit def intMatToDeliteDoubleMat(a: Matrix[Int]) = a.mapRow(v => intVecToDeliteDoubleVec(v))
  implicit def intMatToDeliteFloatMat(a: Matrix[Int]) = a.mapRow(v => intVecToDeliteFloatVec(v))
  implicit def intMatToDeliteLongMat(a: Matrix[Int]) = a.mapRow(v => intVecToDeliteLongVec(v))
  implicit def longMatToDeliteDoubleMat(a: Matrix[Long]) = a.mapRow(v => longVecToDeliteDoubleVec(v))
  implicit def longMatToDeliteFloatMat(a: Matrix[Long]) = a.mapRow(v => longVecToDeliteFloatVec(v))
  implicit def floatMatToDeliteDoubleMat(a: Matrix[Float]) = a.mapRow(v => floatVecToDeliteDoubleVec(v))

  implicit def deliteIntMatToIntMat(a: Matrix[DeliteInt]) = a.mapRow(v => deliteIntVecToIntVec(v))
  implicit def deliteIntMatToDoubleMat(a: Matrix[DeliteInt]) = a.mapRow(v => deliteIntVecToDoubleVec(v))
  implicit def deliteIntMatToFloatMat(a: Matrix[DeliteInt]) = a.mapRow(v => deliteIntVecToFloatVec(v))
  implicit def deliteIntMatToLongMat(a: Matrix[DeliteInt]) = a.mapRow(v => deliteIntVecToLongVec(v))
  implicit def deliteLongMatToLongMat(a: Matrix[DeliteLong]) = a.mapRow(v => deliteLongVecToLongVec(v))
  implicit def deliteLongMatToDoubleMat(a: Matrix[DeliteLong]) = a.mapRow(v => deliteLongVecToDoubleVec(v))
  implicit def deliteLongMatToFloatMat(a: Matrix[DeliteLong]) = a.mapRow(v => deliteLongVecToFloatVec(v))
  implicit def deliteFloatMatToDoubleMat(a: Matrix[DeliteFloat]) = a.mapRow(v => deliteFloatVecToDoubleVec(v))
  implicit def deliteFloatMatToFloatMat(a: Matrix[DeliteFloat]) = a.mapRow(v => deliteFloatVecToFloatVec(v))
  implicit def deliteDoubleMatToDoubleMat(a: Matrix[DeliteDouble]) = a.mapRow(v => deliteDoubleVecToDoubleVec(v))

}

