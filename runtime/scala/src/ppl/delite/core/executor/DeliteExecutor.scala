package ppl.delite.core.executor

import ppl.delite.core.{Config, DeliteDSLType}
import java.util.LinkedList
import java.util.concurrent.ArrayBlockingQueue


trait DeliteExecutor extends Thread with DeliteExecutorConfig {
  // this is the DeliteExecutor thread
  setName("DeliteExecutor");

  val submitQueue = new ArrayBlockingQueue[DeliteDSLType](Config.executorInQueueSize)

  // roots are those proxy that have all their inputs ready to go
  val schedulable = new LinkedList[DeliteDSLType]

  @volatile
  var _shutdown = false

  def submit(proxy: DeliteDSLType) {
    submitQueue.put(proxy)
  }

  def force { }

  def shutdown {
    _shutdown = true
    this.synchronized {
      this.notify
    }
  }
}

