package ppl.delite.core.executor

import ppl.delite.core.{Config, Delite}
import ppl.delite.cuda._

object DeliteGPUThreadPool {

  // Tell whether all GPU devices are ready to go
  @volatile 
  var deviceReady = false

  // Number of initialized GPU execution threads
  @volatile 
  private var initializedWorkers = 0

  val numThreads = if(Config.GPUThreadNumOverride) Config.GPUThreadNum else 1

  println("Starting DeliteGPUThreadPool with " + numThreads + " threads")

  val threads = new Array[DeliteGPUExecutionThread](numThreads)
  var gpuCleanupThread = new Array[DeliteGPUCleanupThread](numThreads)

  def start {
    var idx = 0

    // Initialize Native CUDA library
    System.load("/tmp/cuda/libDeliteCudaDriver.so")
    DeliteCudaDriver.cudaInit
    
    // Start GPU Management Thread & GPU Cleanup Thread
    while(idx < numThreads) {
      val thread = new DeliteGPUExecutionThread(idx)
      thread.start
      threads(idx) = thread

      val cleanupThread = new DeliteGPUCleanupThread(idx)
      cleanupThread.start
      gpuCleanupThread(idx) = cleanupThread

      idx += 1

      // Wait until all GPU ExecutionThreads and GPU CleanupThread are initialized
      while(initializedWorkers < 2*idx) {}
    }

    // All GPU devices are initialized
    deviceReady = true
  }

  // intializedThreads variable is incremented by each exeuctionThread
  // when initialization of each device is done
  def initDone {
	  synchronized {
	  	initializedWorkers = initializedWorkers + 1
	  }
  }

  def shutdown {
    println("Shutting Down GPU ThreadPool")
    // Shutdown GPU Management Thread and GPU Cleanup Thread
    var idx = 0
    while(idx < numThreads) {
      threads(idx).shutdown
      gpuCleanupThread(idx).shutdown
      idx += 1
    }
  }

}

