package ppl.delite.dsl.optiml

import ppl.delite.core.DeliteProxyFactory

/* Description
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Aug 3, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

class IndexVectorWC extends IndexVector {

  override def apply[@specialized(Double)A](block : Int => A)(implicit pFact : DeliteProxyFactory[Vector[A]], c: ClassManifest[A]) : Vector[A] = {
    throw new UnsupportedOperationException("no operations can be performed on wildcard IndexVector")
  }
}