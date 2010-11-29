package ppl.delite.core.ops

import ppl.delite.core.{DeliteUnit, DeliteDSLType}

trait DeliteOP[@specialized(Double)T] {

  def task: T

  /**
   * Method for sequentially executing the task
   */
  def seq: T = task

  //Seq of all input dependencies
  final def getDependencies: Seq[DeliteDSLType] = {
    if (getMutableDeps == null)
      return getImmutableDeps
    if (getImmutableDeps == null)
      return getMutableDeps
    return getImmutableDeps ++ getMutableDeps
  }

  //Seq of input deps that the OP guarantees not to mutate
  //can be set to null rather than empty Seq
  def getImmutableDeps: Seq[DeliteDSLType] = throw new RuntimeException("should have been overridden")

  //Seq of input deps that the OP will mutate
  //can be set to null rather than empty Seq
  def getMutableDeps: Seq[DeliteDSLType] = throw new RuntimeException("should have been overridden")

  //functions specifically for GPU OPs
  def getGPUInputs: List[AnyRef] = null
  def getGPUConsts: List[AnyVal] = null
  def getGPUOutput: DeliteDSLType = null
  def getGPUKernelDims: List[Int] = null
  def getGPUKernelId: List[Int] = null

  def cost: Int = 1

  def submitChunks(num_chunks: Int, submit: DeliteOP[DeliteUnit] => Unit): Unit = null

  def isChunkable = false

  implicit def unitToDeliteUnit(u: Unit) = new DeliteUnit
  implicit def anyRefToDeliteUnit(a: AnyRef) = new DeliteUnit

  /* TODO: stub, implement or remove call-site */
  val associative = false
}

/**
 * TODO: These are stubs that allows the DSLs to compile. Replace with actual implementation or remove call-sites.
 */
abstract class DeliteOP_MultiTask[T <: DeliteDSLType](dependencies: DeliteDSLType*) extends DeliteOP_SingleTask[T]

abstract class DeliteOP_MutableMap[T <: DeliteDSLType](dependencies: DeliteDSLType*) extends DeliteOP_SingleTask[T]
