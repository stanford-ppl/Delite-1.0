package ppl.delite.core.ops

import ppl.delite.core.DeliteDSLType


/**
 * This is a non-mutable single task OP
 */
abstract class DeliteOP_SingleTask[T <: DeliteDSLType](dependencies:  DeliteDSLType*) extends DeliteOP[T] {

  override def getImmutableDeps = dependencies

  override def getMutableDeps = null

}