package ppl.delite.core.executor

import ppl.delite.core._
import OpHelper._

class ExecutorRoundRobin extends DeliteExecutor {
  var sleepTime = 0L
  var sleepTimeIncrement = 1

  /* start the CPU thread pool, we do this in the constructor so that
  most of the init stuff happens before the application starts for timing purposes*/
  DeliteCPUThreadPool.start

  override def run() {
    println("Round Robin Executor is starting")
    //Delite.isExecutor.set(true)

    var windowLeft = 0

    while (!_shutdown || submitQueue.isEmpty == false) {

      /*
      if (submitQueue.isEmpty) { //nothing to process
        this.synchronized {
          this.wait
        }
      }
      */

      windowLeft = Config.executorWindowSize

      // add proxy to the execution tree
      while (windowLeft != 0) {
        val proxy = submitQueue.poll
        if (proxy != null) {
          val isReady = processProxy(proxy)
          //if this is a root, add the proxy to the list of roots
          if (isReady)
            schedulable.add(proxy)
        }
        windowLeft -= 1
      }

      // schedule this window
      scheduleWindow

    }

    // shut down the CPU threads
    DeliteCPUThreadPool.shutdown

    println("Executor is finishing")
  }

  var scheduleOnThisNext = 0
  val numThreads = DeliteCPUThreadPool.numThreads
  val numChunks = numThreads

  def scheduleWindow {
    while (!schedulable.isEmpty) {
      val proxy = schedulable.pop
      // schedule it
      proxy.scheduled = true
      DeliteCPUThreadPool.submitForExecution(proxy, scheduleOnThisNext)
      // setup the next thread to be schedule
      // TODO: need fast modding op from nathan
      scheduleOnThisNext = (scheduleOnThisNext + 1) % numThreads

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

}
