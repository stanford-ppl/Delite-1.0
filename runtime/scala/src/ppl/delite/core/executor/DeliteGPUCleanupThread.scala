package ppl.delite.core.executor

import java.util.concurrent.ArrayBlockingQueue
import ppl.delite.core.{Delite, Config, DeliteDSLType}
import ppl.delite.dsl.optiml.specialized._
import ppl.delite.dsl.optiml.Matrix
import ppl.delite.cuda.DeliteCuda
import ppl.delite.nativeGPU.GPUable

class DeliteGPUCleanupThread(idx: Int) extends Thread {
  
  @volatile
  private var _shutdown = false

  // This variable is set true when all dependencies are cleared
  @volatile
  var forcing_done = false
  
  // The queue for handling force request
  val forceQueue = new ArrayBlockingQueue[DeliteDSLType](Config.executorInQueueSize)

  // Call myself something descriptive
  setName("GPU-Cleanup-Thread")

  override def run = {
    Delite.isGPUManager.set(true)
    
    // Make sure Delite knows I am an executor thread
    //Delite.isExecutor.set(true)
    println("Starting GPU Cleanup Thread: " + idx)

    // TODO: Need this?
    var useful = false

	  // Let DeliteCPUThreadPool know that initilazation of this worker is done
	  DeliteGPUThreadPool.initDone

    while(!_shutdown || forceQueue.isEmpty == false) {
      val proxy = forceQueue.peek
      if(proxy != null) {
        //val num = if(proxy.antiDeps==null) (proxy.inputs.size) else (proxy.antiDeps.size+proxy.inputs.size)
        //println("forcing " + proxy.op.toString)
        //if(proxy.op.getMutableDeps != null)
        //  println(proxy.op.getMutableDeps.size + " Mutable Deps")
        proxy.header
        // Now the dependencies are resolved (All former OPs are done)
        forceQueue.poll

        //println(num + " Dependencies Resolved!")
        //println(proxy.op.toString + ", " + num +   " Dependencies Resolved!")
        forcing_done = true
      }


    }

    println("Exiting GPU Cleanup Thread")
  }

  def gpuOpHeader(proxy: DeliteDSLType) {
    forcing_done = false
    forceQueue.put(proxy)
  }
  
  def shutdown {
    _shutdown = true
    this.synchronized {
      this.notify
    }
  }
}
