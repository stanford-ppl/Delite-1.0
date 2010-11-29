package ppl.delite.core.executor

import ppl.delite.core.Config
import ppl.delite.core.DeliteDSLType

object DeliteCPUThreadPool {

  // Tell whether all GPU devices are ready to go
  @volatile 
  var deviceReady = false
  
  // Number of initialized CPU execution threads
  @volatile 
  private var initializedWorkers = 0

  val numThreads = Config.CPUThreadNum
  println("Starting DeliteCPUThreadPool with " + numThreads + " threads")

  val threads = new Array[DeliteCPUExecutionThread](numThreads)

  def start {
    var idx = 0
    while(idx != numThreads) {
      val thread = new DeliteCPUExecutionThread(idx)
      thread.start
      threads(idx) = thread
      idx += 1
    }

    // Wait until all CPU ExecutionThreads are initialized
    while(initializedWorkers < numThreads) { }

    // All CPU devices are initialized
    deviceReady = true
  }

  def submitForExecution(proxy: DeliteDSLType) {
    throw new RuntimeException("this has not been implemented yet, need to specify target thread")
  }


  def submitForExecution(proxy: DeliteDSLType, tid: Int){
    threads(tid).taskQueue.put(proxy)
    threads(tid).synchronized {
      threads(tid).notify
    }
  }

  // intializedThreads variable is incremented by each exeuctionThread
  // when initialization of each device is done
  def initDone {
	  synchronized {
	  	initializedWorkers = initializedWorkers + 1
	  }
  }

  def shutdown {
    println("Shutting Down ThreadPool")
    var idx = 0
    while(idx != numThreads) {
      threads(idx).shutdown
      idx += 1
    }
  }

}

