package ppl.delite.core

/**
 * DeliteFunc is a wrapper around functions that captures dependencies inside the function closure.
 * The closure dependency plugin should wrap every function parameter passed to a DeliteOp in a DeliteFunc.
 */

/* Machinery for allowing functions to be easily wrapped inside a DeliteFunc. */
object DeliteFunc {
  // allows constructions for different number of parameters
  def apply[@specialized(Double,Float,Int,Unit) R](f: => R, fdeps: DeliteDSLType*) = new DeliteFuncLazy(f, fdeps)

  def apply[@specialized(Double,Float,Int,Unit) R](f: () => R, fdeps: DeliteDSLType*) = new DeliteFunc0(f, fdeps)

  def apply[@specialized(Double,Float,Int) T1,@specialized(Double,Float,Int,Unit) R](f: T1 => R, fdeps: DeliteDSLType*) = new DeliteFunc1(f, fdeps)

  def apply[@specialized(Double,Float,Int) T1,@specialized(Double,Float,Int) T2,@specialized(Double,Float,Int,Unit) R](f: (T1, T2) => R, fdeps: DeliteDSLType*) = new DeliteFunc2(f, fdeps)

  def apply[T1, T2, T3, R](f: (T1, T2, T3) => R, fdeps: DeliteDSLType*) = new DeliteFunc3(f, fdeps)

  def apply[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R, fdeps: DeliteDSLType*) = new DeliteFunc4(f, fdeps)

  def apply[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R, fdeps: DeliteDSLType*) = new DeliteFunc5(f, fdeps)

  def apply[T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => R, fdeps: DeliteDSLType*) = new DeliteFunc6(f, fdeps)

  def apply[T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R, fdeps: DeliteDSLType*) = new DeliteFunc7(f, fdeps)

  def apply[T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R, fdeps: DeliteDSLType*) = new DeliteFunc8(f, fdeps)

  def apply[T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R, fdeps: DeliteDSLType*) = new DeliteFunc9(f, fdeps)
}

abstract class DeliteFunc {
  var deps: Seq[DeliteDSLType]
}

class DeliteFuncLazy[@specialized(Double,Float,Int,Unit) +R](f: => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function0[R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply() : R = f
}

class DeliteFunc0[@specialized(Double,Float,Int,Unit) +R](f:() => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function0[R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply() : R = f()
}

class DeliteFunc1[@specialized(Double,Float,Int) -T1,@specialized(Double,Float,Int,Unit) +R](f: T1 => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function1[T1,R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply(v1: T1) : R = f(v1)
}

class DeliteFunc2[@specialized(Double,Float,Int) -T1,@specialized(Double,Float,Int) -T2,@specialized(Double,Float,Int,Unit) +R](f: (T1,T2) => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function2[T1,T2,R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply(v1: T1, v2: T2) : R = f(v1, v2)
}

class DeliteFunc3[-T1,-T2,-T3,+R](f: (T1,T2,T3) => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function3[T1,T2,T3,R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply(v1: T1, v2: T2, v3: T3) : R = f(v1, v2, v3)
}

class DeliteFunc4[-T1,-T2,-T3,-T4,+R](f: (T1,T2,T3,T4) => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function4[T1,T2,T3,T4,R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply(v1: T1, v2: T2, v3: T3, v4: T4) : R = f(v1, v2, v3, v4)
}

class DeliteFunc5[-T1,-T2,-T3,-T4,-T5,+R](f: (T1,T2,T3,T4,T5) => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function5[T1,T2,T3,T4,T5,R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5) : R = f(v1, v2, v3, v4, v5)
}

class DeliteFunc6[-T1,-T2,-T3,-T4,-T5,-T6,+R](f: (T1,T2,T3,T4,T5,T6) => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function6[T1,T2,T3,T4,T5,T6,R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6) : R = f(v1, v2, v3, v4, v5, v6)
}

class DeliteFunc7[-T1,-T2,-T3,-T4,-T5,-T6,-T7,+R](f: (T1,T2,T3,T4,T5,T6,T7) => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function7[T1,T2,T3,T4,T5,T6,T7,R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7) : R = f(v1, v2, v3, v4, v5, v6, v7)
}

class DeliteFunc8[-T1,-T2,-T3,-T4,-T5,-T6,-T7,-T8,+R](f: (T1,T2,T3,T4,T5,T6,T7,T8) => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function8[T1,T2,T3,T4,T5,T6,T7,T8,R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8) : R = f(v1, v2, v3, v4, v5, v6, v7, v8)
}

class DeliteFunc9[-T1,-T2,-T3,-T4,-T5,-T6,-T7,-T8,-T9,+R](f: (T1,T2,T3,T4,T5,T6,T7,T8,T9) => R, fdeps: Seq[DeliteDSLType]) extends DeliteFunc with Function9[T1,T2,T3,T4,T5,T6,T7,T8,T9,R] {
  var deps : Seq[DeliteDSLType] = fdeps
  def apply(v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9) : R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9)
}
