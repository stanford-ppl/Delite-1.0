/* BestEffortVector
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * modified: 11/6/09
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.delite.dsl.optiml.unreliable

import ppl.delite.core.include._
import ppl.delite.core.appinclude._
import ppl.delite.core.ops._
import ppl.delite.core._
import ppl.delite.dsl.optiml._
import reflect.ClassManifest

object BestEffortVector {
  ///////////////////////
  // user visible methods

  def apply[A : ClassManifest](len: Int) : BestEffortVector[A] = newVector[A](len)

  //////////////////////
  // vector construction

  private def newVector[A](len: Int, is_row: Boolean = true)(implicit m: ClassManifest[A]): BestEffortVector[A] = {
    m match {
      case ClassManifest.Int => new BestEffortIntVectorImpl(is_row, len).asInstanceOf[BestEffortVector[A]]
      case _ => new BestEffortVectorImpl[A](is_row, len)
    }
  }
  
  class ProxyFactory[T](implicit c: ClassManifest[T]) extends DeliteProxyFactory[BestEffortVector[T]] {
    override def newProxy = new BestEffortVectorImpl[T]
  }


}

trait BestEffortVector[@specialized(Double,Float,Int) T] extends Vector[T] {
  import BestEffortVector._

  def getPolicy : BestEffortPolicy[T]
  def setPolicy(p: BestEffortPolicy[T]) : Unit
  
  def skip(i: Int) : Boolean

  def always_update(n: Int, x: T)

  def be_update(n: Int, x: () => T)
}


