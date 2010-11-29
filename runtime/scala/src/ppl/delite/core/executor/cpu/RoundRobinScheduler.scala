package ppl.delite.core.executor.cpu

import ppl.delite.core.executor.OpHelper._
import java.util.LinkedList
import ppl.delite.core._
import executor.{DeliteExecutorConfig, DeliteCPUThreadPool, RootSet}
import ppl.delite.core.ops.DeliteOP

/**
 * Author: Kevin J. Brown
 * Date: Apr 9, 2010
 * Time: 6:19:13 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class RoundRobinScheduler extends DeliteExecutorConfig {

  println("Round Robin Scheduler is starting")

  var scheduleOnThisNext = 0
  val numThreads = DeliteCPUThreadPool.numThreads
  val numChunks = numThreads
  val window = Config.executorWindowSize
  var windowLeft = window
  val schedulable = new LinkedList[DeliteDSLType]

  def submit(proxy: DeliteDSLType) = process(proxy)

  def process(proxy: DeliteDSLType) {
    val isReady = processProxy(proxy)
    if (isReady) schedulable.add(proxy)
    windowLeft -= 1
    if (windowLeft == 0) {
      scheduleWindow
      windowLeft = window
    }
  }

  def force = scheduleWindow

  def scheduleWindow {
    while (!schedulable.isEmpty) {
      val proxy = schedulable.poll
      // schedule it
      proxy.scheduled = true
      if (proxy.op.isChunkable) {
        proxy.op.submitChunks(numChunks, submitChunk(proxy, _))
        DeliteCPUThreadPool.submitForExecution(proxy, scheduleOnThisNext)
        scheduleOnThisNext = (scheduleOnThisNext + 1) % numThreads
      }
      else {
        DeliteCPUThreadPool.submitForExecution(proxy, scheduleOnThisNext)
        scheduleOnThisNext = (scheduleOnThisNext + 1) % numThreads
      }
      //now enqueue its schedulable children
      enqueueSchedulableChildren(proxy)
    }
  }

  def enqueueSchedulableChildren(proxy: DeliteDSLType) {
    val outputs = proxy.outputs
    val iter = outputs.iterator
    while (iter.hasNext) {
      val output = iter.next
      if (output.isSchedulable) {
        schedulable.add(output)
      }
    }
  }

  def submitChunk(orig: DeliteDSLType, op: DeliteOP[DeliteUnit]) {
    val chunk = createChunkProxy(orig, op)
    DeliteCPUThreadPool.submitForExecution(chunk, scheduleOnThisNext)
    scheduleOnThisNext = (scheduleOnThisNext + 1) % numThreads
  }

  def start = DeliteCPUThreadPool.start

  def shutdown = DeliteCPUThreadPool.shutdown

}