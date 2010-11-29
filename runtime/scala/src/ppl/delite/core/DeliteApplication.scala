package ppl.delite.core

import executor._
import configs._
import cpu._

trait DeliteApplication {

//  var appTime:Long = _
//  var phaseTime:Long = _
//
//  resetTimer


  def initialize {
    // pick the right executor, executors are responsible for starting and stopping
    // any other threads they need (e.g. ThreadPools)
    if (!(Config.debugEnabled && Config.bypassExecutor)) {
      Delite.executor = Config.executorType match {
        case "default" => new SimpleClusterScheduler
        case "round-robin" => new RoundRobinScheduler
        case "sequential" => new SequentialScheduler
        case "gpu" => new ExecutorGPU
        case "visualizer" => new ExecutorVisualizer
        case et@_ => throw new RuntimeException("'" + et + "' is not a valid Executor Type")
      }
    }
    else {
      println("No Executor")
    }

    // start the executor, run the application and then shutdown the executor
    if(Config.debugEnabled!=true || Config.bypassExecutor==false) Delite.executor.start
    println("runtime initialization complete")
  }

  def shutdown {
    println("application execution complete")
    if(Config.debugEnabled!=true || Config.bypassExecutor==false) Delite.executor.shutdown
  }

  final def main(args: Array[String]) {
    initialize
//    resetTimer
    run(args)
    shutdown        
  }

//  private def resetTimer  {
//    appTime = System.currentTimeMillis
//    phaseTime = appTime
//  }

//  def logElapsed(): Unit = logElapsed("")

  def run(args: Array[String])

//  def logElapsed(msg: String) {
//    val ff = System.currentTimeMillis
//    val appEl = ((ff - appTime) / 1000.0) formatted("%.6f") + "s"
//    val phsEl = ((ff - phaseTime) / 1000.0) formatted("%.6f") + "s"
//    println( "[" + msg + "]\n" + "time elapsed: " + appEl + " (" + phsEl + ") " )
//    phaseTime = System.currentTimeMillis
//  }
}
