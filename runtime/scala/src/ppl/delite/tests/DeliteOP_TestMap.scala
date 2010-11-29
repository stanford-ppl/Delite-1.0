/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 8, 2010
 * modified: Feb 8, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.tests

import ppl.delite.core.ops.DeliteOP_Map
import ppl.delite.core.{DeliteCollectionFactory, DeliteCollection, DeliteProxyFactory}
/*
case class DeliteOP_TestMap[A,B,C[X] <: DeliteCollection[X]](val coll: DeliteCollection[A], val func: A => B)
    (implicit builder: DeliteCollectionFactory[C], pFact: DeliteProxyFactory[C[B]], m: ClassManifest[B])
    extends DeliteOP_Map[A,B,C]{

    def testChunks(num_chunks: Int) = getChunks(num_chunks)
}
*/
