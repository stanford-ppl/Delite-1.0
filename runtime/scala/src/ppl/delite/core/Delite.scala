package ppl.delite.core


import executor.DeliteExecutorConfig
import ops.DeliteOP

object Delite {

  private val mainThread = Thread.currentThread
  
  def isMainThread: Boolean = Thread.currentThread == mainThread


  // Only GPU Management Thread will have this variable true
  // Used in proxy force function
  val isGPUManager = new ThreadLocal[Boolean] {
    override def initialValue = false
  }

  var init = false;

  var executor: DeliteExecutorConfig = null

  def force {
    if (isMainThread) executor.force
  }


  def run[T <: DeliteDSLType](op: DeliteOP[T])(implicit proxyFactory: DeliteProxyFactory[T]):T =  {
    // If debug mode is enabled and you bypassing the executor has been requested, return the op task
    if(Config.debugEnabled == true && Config.bypassExecutor == true) return op.seq
    //TODO: insert a "fast path" here which allows us to compute cheap ops locally (that we don't want to defer) while still ordering them properly

    if(isMainThread && init == false) {
      // create a new proxy that will hold the op
      val proxy = proxyFactory.newProxy
      // set the op
      proxy.op = op.asInstanceOf[DeliteOP[proxy.DSLType]]
      // submit the proxy and op to the executor
      //println("sending proxy " + proxy + " to executor")

      executor.submit(proxy)

      /*
      if (executor.submitQueue.size == 1) { //queue was empty
        executor.synchronized {
          executor.notify
        }
      }
      */

      return proxy
    }
    else {
      /*
      if(init == false) {
        println("Some nested deferral going on here on op " + op)
        Thread.dumpStack
      }
      */
      op.seq
    }

  }

}
