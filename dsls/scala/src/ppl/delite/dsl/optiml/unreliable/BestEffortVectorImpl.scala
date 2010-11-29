/* array-backed, double precision floating point Vector
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * modified: 4/14/09
 * credits:  Nathan Bronson (from PBuffer.scala, HeapDoublePBuffer.scala)
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.delite.dsl.optiml.unreliable

import ppl.delite.core.include._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.core.{DeliteDSLType, DeliteCore}
import ppl.delite.dsl.optiml._

private[optiml] class BestEffortVectorImpl[@specialized(Double,Float,Int) T : ClassManifest]
  extends VectorImpl[T] with BestEffortVector[T] {

  def this(row_vec: Boolean, len: Int) = {
    this()
    init(row_vec, len)
    policy = new DefaultBestEffortPolicy[T](len)
    isComputed = true
  }

  private var policy : BestEffortPolicy[T] = null

  def getPolicy = policy
  
  def setPolicy(p: BestEffortPolicy[T]) = {
    policy = p
  }

  // should we skip an operation on this element?
  def skip(n: Int) : Boolean = {
    policy.skip(_data, n)
  }

  def always_update(n: Int, x: T){
    _data(n) = x
  }

  override def update(n: Int, x: T) = always_update(n, x)
  
  def be_update(n: Int, x: () => T){
    //println("called best effort update")
    if (skip(n) == false){
      _data(n) = x()
    }
  }
  
}
