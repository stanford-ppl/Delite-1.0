package ppl.delite.core.ops

import ppl.delite.core.DeliteDSLType


/**
 * This is an OP that mutates some of its inputs. the first set of inputs are not mutated
 * while the second set are
 *
 *
 *
 */
abstract class DeliteOP_MutableSingleTask[T <: DeliteDSLType](immutable: DeliteDSLType*)(mutable: DeliteDSLType*) extends DeliteOP[T]  {

  //override def getDependencies = nonMutable ++ mutable

  override def getImmutableDeps = immutable

  override def getMutableDeps = mutable

}