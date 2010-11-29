package ppl.delite.core.executor

import ppl.delite.core.{DeliteDSLType, Config, Delite}
import java.io.{FileReader, BufferedReader}

class ExecutorGPU extends DeliteExecutor {
  var sleepTime = 0L
  var sleepTimeIncrement = 1

  /* start the CPU/GPU thread pool, we do this in the constructor so that
  most of the init stuff happens before the application starts for timing purposes*/
  DeliteCPUThreadPool.start
  DeliteGPUThreadPool.start

  // Get the list of OPs to ship to GPU
  val gpuEnabledOPs = readGPUOPs
  val numGPUOPs = gpuEnabledOPs.length
  println("Number of GPU enabled OPs is " + numGPUOPs)

  while(!DeliteCPUThreadPool.deviceReady) {}
  while(!DeliteGPUThreadPool.deviceReady) {}

  def readGPUOPs():Array[String] = {
    var gpuEnabledOPs = new Array[String](0)

    if(Config.gpuOPsFileName != null) {
      //val filename = System.getProperty("user.dir") + "/runtime/scala/src/ppl/delite/cuda/" + Config.gpuOPsFileName
      val filename = System.getProperty("delite.basedir") + "/runtime/scala/src/ppl/delite/cuda/" + Config.gpuOPsFileName
      val reader = new BufferedReader(new FileReader(filename))
      var line = reader.readLine
      while (line != null) {
        gpuEnabledOPs = gpuEnabledOPs ++ Array(line)
        line = reader.readLine
      }
      reader.close
    }
    else
      println("No GPU OPs enabled")
    gpuEnabledOPs
  }

  // Check whether given OP is GPU enabled or not
  def checkGPUOP(name:String):Boolean = {
    //println("STR : " + name)
    var i = 0
    while(i < numGPUOPs) {
      if(gpuEnabledOPs(i).equals(name)) {
        return true
      }
      i += 1
    }
    false
  }

  override def run() {
    println("GPU Executor is starting")
    //Delite.isExecutor.set(true)

    var windowLeft = 0
    var devIndex = 0
    val numGPU = Config.GPUThreadNum
    
	  while (!_shutdown || submitQueue.isEmpty == false) {
	    // Currently, No scheduling is performed.
	    // simply send proxies to GPU if the OP has kernel for it.

      val proxy = submitQueue.poll

	    if(proxy != null) {
        devIndex = (devIndex+1)%numGPU
        
        OpHelper.processProxy(proxy)
        //if (proxy.op.getGPUKernelId != null) { // Send to GPU
        //if(false) {
        if(checkGPUOP(proxy.op.getClass.getName)) {
         // This is a blocking call when the queue(ArrayBlockingQueue) is full
          proxy.isScheduledOnGPU = true
          proxy.GPUdevIdx = devIndex
          DeliteGPUThreadPool.threads(devIndex).taskQueue.put(proxy)
          proxy.scheduled = true
        }
        else { // No corresponding GPU kernel for the OP. 
		  DeliteCPUThreadPool.submitForExecution(proxy,0)
          proxy.scheduled = true
		  //executeProxy(proxy)
        //}
	    }
		}
    }

    // shut down the CPU/GPU threads
    DeliteCPUThreadPool.shutdown
    DeliteGPUThreadPool.shutdown

    println("GPU Executor is finishing")
  }


  def executeProxy(proxy: DeliteDSLType) {
	  proxy.header
	  val value = proxy.op.seq
	  proxy.synchronized {
	  	proxy.cvalue = value
	  	proxy.concretize
	  	proxy.isComputed = true
		if(proxy.forcing)
			proxy.notifyAll
	  }
	  clearDeps(proxy)
	  cleanup(proxy)
  }

    def clearDeps(proxy: DeliteDSLType) {
	    if (proxy.op.getImmutableDeps != null) {
	      val iter = proxy.op.getImmutableDeps.iterator
          while (iter.hasNext) {
	        val dep = iter.next
	        dep.synchronized {
	          dep.outstandingOPs.remove(proxy) //completed, so remove from outstanding list
            }
	      }
      }
      if (proxy.op.getMutableDeps != null) {
        val iter = proxy.op.getMutableDeps.iterator
        while (iter.hasNext) {
          val dep = iter.next
          dep.synchronized {
            dep.outstandingOPs.remove(proxy)
            if (dep.lastMutatingOP == proxy) { //speedup future pure ops
	            dep.lastMutatingOP = null
            }
          }
        }
      }
	}

	def cleanup(proxy: DeliteDSLType) {
	   val iter = proxy.inputs.iterator
	   while (iter.hasNext) {
	     val input = iter.next
	     input.outputs.remove(proxy)
	   }
	   proxy.op = null
	   proxy.antiDeps = null
	   proxy.inputs = null
    }

}
