/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Jan 27, 2010
 * modified: Jan 27, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.core

object include {
  /*
   TODO: stubs! Replace with actual implementations or remove call-sites.
   */
  var defer = false

  var developmentMode = false
  var researchMode = false
  
  var recentEvalIDs: scala.collection.immutable.Set[Int] = scala.collection.immutable.TreeSet.empty[Int]
  
  val delite = DeliteCore
  
  def getUserLineNumber = {}
}