/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Nov 8, 2009
 * modified: Nov 8, 2009
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.dsl.optiml.unreliable

import ppl.delite.core.DeliteDSLType
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.primitive.{DeliteDouble, DeliteInt}

object ConvergingBestEffortPolicy{
  /* common difference functions */
  implicit val intDiff : (Int, Int) => Double = (a: Int, b: Int) => Math.abs(a - b)
  implicit val doubleDiff : (Double, Double) => Double = (a: Double, b: Double) => Math.abs(a-b)
}

class ConvergingBestEffortPolicy[@specialized(Double,Float,Int) T](length: Int)(implicit difference: (T, T) => Double, c: ClassManifest[T])
  extends BestEffortPolicy[T] {
  import ConvergingBestEffortPolicy._

  private val threshold = .1
  private val N = 40
  private var shadow : Array[T] = null
  private val lastUpdated = new Array[Int](length)

  def skip(elements : Array[T], n: Int) : Boolean = {
    if (shadow == null) shadow = (new Array[T](0) ++ elements)
    if (lastUpdated(n) > N){
      // this point has remained the same over the last N iterations
      //println("found converged pt!")
      return true
    }

    if (difference(elements(n), shadow(n)) > threshold){
      // not converged
      shadow(n) = elements(n)
      lastUpdated(n) = 0
      //println("found not converged pt")
    }
    else{
      //println("found element same as before")
      shadow(n) = elements(n)
      lastUpdated(n) += 1
    }

    return false
  }
}