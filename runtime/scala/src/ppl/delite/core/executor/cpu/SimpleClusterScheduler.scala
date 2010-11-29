package ppl.delite.core.executor.cpu

import ppl.delite.core.executor.OpHelper._
import java.util.{LinkedList, ArrayList}
import ppl.delite.core.ops.DeliteOP
import ppl.delite.core._
import executor.{DeliteExecutorConfig, DeliteCPUThreadPool, ActiveBlock}

/**
 * Author: Kevin J. Brown
 * Date: Apr 9, 2010
 * Time: 10:37:41 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class SimpleClusterScheduler extends DeliteExecutorConfig {

  println("Simple Cluster Scheduler is starting")

  val windowSize = Config.executorWindowSize
  var windowLeft = windowSize
  val scheduleQueue = new LinkedList[DeliteDSLType]

  def submit(proxy: DeliteDSLType) = process(proxy)

  def process(proxy: DeliteDSLType) {
    processProxy(proxy)
    scheduleQueue.add(proxy)
    windowLeft -= 1
    if (windowLeft == 0) {
      scheduleWindow
      windowLeft = windowSize
    }
  }

  def force = scheduleWindow

  var numThreads = DeliteCPUThreadPool.numThreads
  var nextThread = numThreads - 1
  var lastProxy: DeliteDSLType = null
  val numChunks = numThreads
  var chunkThread = 0

  def scheduleWindow {
    while (!scheduleQueue.isEmpty) {
      val proxy = scheduleQueue.poll
      proxy.scheduled = true
      if (proxy.op.isChunkable) {
        chunkThread = 0
        proxy.op.submitChunks(numChunks, submitChunk(proxy, _))
        chunkThread = 0
        DeliteCPUThreadPool.submitForExecution(proxy, chunkThread)
      }
      else {
        if (proxy.inputs.contains(lastProxy)) {
          DeliteCPUThreadPool.submitForExecution(proxy, nextThread)
        }
        else {
          nextThread = (nextThread + 1) % numThreads
          DeliteCPUThreadPool.submitForExecution(proxy, nextThread)
        }
      }
      lastProxy = proxy
    }
  }

  def submitChunk(orig: DeliteDSLType, op: DeliteOP[DeliteUnit]) {
    chunkThread = (chunkThread + 1) % numThreads
    val chunk = createChunkProxy(orig, op)
    DeliteCPUThreadPool.submitForExecution(chunk, chunkThread)
  }

  def start = DeliteCPUThreadPool.start

  def shutdown = DeliteCPUThreadPool.shutdown

}