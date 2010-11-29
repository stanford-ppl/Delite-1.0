/* Implements a simple random sampling BestEffort policy. This policy should
 * only be used when a significant degree of redundancy is expected in the input
 * data.
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
import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml._

class RandomBestEffortPolicy[@specialized(Double,Float,Int) T](length: Int, skipPct : Double) extends BestEffortPolicy[T] {
  val randomSkip = Vector.rand(length).map(e => e > skipPct)

  def skip(elements : Array[T], n: Int) = randomSkip(n)
}