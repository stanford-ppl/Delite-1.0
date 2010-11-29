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

class DefaultBestEffortPolicy[@specialized(Double,Float,Int) T](length: Int) extends RandomBestEffortPolicy[T](length, .5)
  with BestEffortPolicy[T] 