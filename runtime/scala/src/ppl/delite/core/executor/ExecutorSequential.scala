package ppl.delite.core.executor

import ppl.delite.core.{DeliteDSLType, Delite}

class ExecutorSequential extends DeliteExecutor {


  // this is the version used to optimize overhead
  override def run() {
    println("Sequential Executor is starting")
    //Delite.isExecutor.set(true)

    while(!_shutdown || submitQueue.isEmpty == false) {
      val proxy = submitQueue.poll
      if(proxy != null) {
        processProxy(proxy)
      }
    }
  }

  def processProxy(proxy: DeliteDSLType) {
    proxy.scheduled = true
    proxy.cvalue = proxy.op.seq
    proxy.concretize
    proxy.isComputed = true
    proxy.op = null

    //check if you need to wake up main thread
    proxy.synchronized {
      if(proxy.forcing)
        proxy.notify
    }

  }

}
