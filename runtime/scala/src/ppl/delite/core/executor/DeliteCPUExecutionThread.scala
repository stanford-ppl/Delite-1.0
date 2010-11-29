package ppl.delite.core.executor

import java.util.concurrent.ArrayBlockingQueue
import ppl.delite.core.{DeliteDSLType, Config, Delite}

class DeliteCPUExecutionThread(val id:Int) extends Thread {


  @volatile
  private var _shutdown = false;

  val taskQueue = new ArrayBlockingQueue[DeliteDSLType](Config.executorInQueueSize)

  // Call myself something descriptive
  setName("CPU-Exec-Thread[" + id + "]")



  override def run = {
    // Make sure Delite knows I am an executor thread
    //Delite.isExecutor.set(true)

//    println("Starting Executor Thread: " + id )


    var useful = false
    //var sleepCount = 0
    var spinsLeft = Config.spinBeforeSleep
    val letSleep = Config.letWorkersSleep

	  // Let DeliteCPUThreadPool know that initilazation of this worker is done
	  DeliteCPUThreadPool.initDone
    
    while(!_shutdown || taskQueue.isEmpty == false) {
      useful = false

      while(taskQueue.isEmpty == false) {
        val proxy = taskQueue.poll
        executeProxy(proxy)
      }


      if(!useful) {
        spinsLeft -= 1
      }
      if(spinsLeft == 0 && letSleep) {
        //sleepCount += 1
        this.synchronized {
          if (taskQueue.isEmpty) this.wait
        }
        spinsLeft = Config.spinBeforeSleep
      }
      
    }

//    println("Exiting Executor Thread: " + id)
    //println("sleepCount = " + sleepCount)
  }

  def executeProxy(proxy: DeliteDSLType) {
    //execute sequentially (this is either a sequential op or a sequential chunk)
    proxy.header
    val value = proxy.op.seq
    proxy.synchronized {
      proxy.cvalue = value
      proxy.concretize
      proxy.isComputed = true
      if (proxy.forcing)
        proxy.notifyAll
    }

    clearDeps(proxy)
    cleanup(proxy)

    /*
    val parent = proxy.parent.asInstanceOf[DeliteProxy[DeliteDSLType]]
    if (parent == null) {
      clearDeps(proxy)
      cleanup(proxy)
    }
    else {
      parent.synchronized {
        parent.childrenLeft -= 1
      }
      if (parent.childrenLeft == 0) {
        parent.synchronized {
          parent.cvalue = parent.op.task //the task is definitely complete here, grab the output
          parent.concretize
          parent.isComputed = true
          if (parent.forcing)
            parent.notifyAll
        }
        clearDeps(parent)
        cleanup(parent)
      }
    }
    */

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
            if (dep.forcing) dep.notifyAll
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
    //proxy.outputs = null
  }

  def shutdown {
//    println("Shutting down Thread: "+ id)
    _shutdown = true
    this.synchronized {
      this.notify
    }
  }


  
}
