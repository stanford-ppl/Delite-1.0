/* BestEffortPolicy defines the interface for the policies used with
 * BestEffortVector.
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

/**
 * All BestEffortPolicies should be immutable.
 */
trait BestEffortPolicy[@specialized(Double,Float,Int) T] {
  // should a computation on  elements[n] be skipped?
  def skip(elements : Array[T], n: Int) : Boolean
}