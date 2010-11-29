/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 1, 2010
 * modified: Feb 1, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.core

trait DeliteCollection[@specialized(Double,Float,Int)A] extends DeliteDSLType {
  type Shape

  /* A collection-specific member used to contain parameters other than size required
     to instantiate an instance of the collection. */
  def shape : Option[Shape] = None

  /* the number of elements in this collection */
  def size : Int

  /* an iterator over the requested range of elements, inclusive */
  def chunk(start: Int, end: Int) : Iterator[A]

  /* this(n) = x */
  // TODO: why can't we have multiple abstract 'update' functions in multiple traits?
  // TODO: we shouldn't require an update, just an addition operator (++=), e.g. Buildable
  def dc_update(n: Int, x: A)

  def dc_apply(n: Int) : A
}
