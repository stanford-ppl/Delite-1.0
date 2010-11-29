package ppl.delite.core.executor

import ppl.delite.core.ops.{DeliteOP, DeliteOP_SingleTask}
import ppl.delite.core.{DeliteUnit, DeliteDSLType}

/**
 * Author: Kevin J. Brown
 * Date: Mar 25, 2010
 * Time: 5:03:24 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

/**
 * This object contains common methods that multiple schedulers will likely want to use
 */

object OpHelper {

  /*
  def analyze[T <: DeliteDSLType](proxy: DeliteProxy[T], num_chunks: Int) {
    val op = proxy.op
    op match {
      case single: DeliteOP_SingleTask[T] => submit(proxy)
      case mutable: DeliteOP_MutableSingleTask[T] => //
      //for the cases with more complicated Type structure we assume sanity
      case map: DeliteOP_Map[_,_,_] => //mapChunk(map, num_chunks)
      case zip: DeliteOP_ZipWith2[_,_,_,_] => //zip.getChunks(num_chunks)
      case reduce: DeliteOP_Reduce[_,_] => {
        //reduce.getChunks(num_chunks)
        //final reduce
      }
      case _ => throw new RuntimeException("OP type not recognized")
    }
  }
  */

  /**
   * Processes the proxy's dependency list(s) and determines if it's ready to execute
   */
  def processProxy[T <: DeliteDSLType](proxy: DeliteDSLType): Boolean = {
    var proxyInputsReady = true
    if (proxy.op.getImmutableDeps != null) {
      val iter = proxy.op.getImmutableDeps.iterator
      while (iter.hasNext) {
        val dep = iter.next
        proxy.inputs.add(dep)
        dep.outputs.add(proxy)
        if (!dep.scheduled) {
          proxyInputsReady = false
        }
        //add this proxy to the dep's outstanding list
        dep.synchronized {
          dep.outstandingOPs.add(proxy)
          if (dep.lastMutatingOP != null) {
            //add synchronization on latest mutating op
            proxy.antiDeps.add(dep.lastMutatingOP)
          }
        }
      }
    }
    if (proxy.op.getMutableDeps != null) {
      val iter = proxy.op.getMutableDeps.iterator
      while (iter.hasNext) {
        val dep = iter.next
        proxy.inputs.add(dep)
        dep.outputs.add(proxy)
        if (!dep.scheduled) {
          proxyInputsReady = false
        }
        dep.synchronized {
          //add synchronization on all previous ops
          proxy.antiDeps.addAll(dep.outstandingOPs)
          dep.outstandingOPs.add(proxy)
          dep.lastMutatingOP = proxy
        }
      }
    }
    return proxyInputsReady
  }

  /* creates a new Proxy for a Data-Parallel Chunk
      these secondary chunks must return DeliteUnit
   */
  def createChunkProxy(orig: DeliteDSLType, op: DeliteOP[DeliteUnit]): DeliteDSLType = {
    val chunk = new DeliteUnit
    chunk.op = op
    chunk.inputs.addAll(orig.inputs)
    //chunk.outputs = orig.outputs
    chunk.antiDeps.addAll(orig.antiDeps)
    chunk
  }

}