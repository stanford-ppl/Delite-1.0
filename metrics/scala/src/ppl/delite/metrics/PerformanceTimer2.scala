package ppl.delite.metrics

import collection.mutable.HashMap

/**
 * User: Anand Atreya
 */

object PerformanceTimer2
{
  val currentTimer = new HashMap[String, Long]
  val times = new HashMap[String, Double]

  def start(component: String, printMessage: Boolean = true) {
    if (!times.contains(component)) {
      times += component -> 0.0
    }
    if (printMessage) println("[METRICS]: Timing " + component + " started")
    currentTimer += component -> System.nanoTime
  }

  def stop(component: String, printMessage: Boolean = true) {
    val x = (System.nanoTime - currentTimer(component)) / 1000000000.0
    times(component) += x
    if (printMessage) println("[METRICS]: Timing " + component + " stopped")
  }

  def summarize(component: String) {
    val total = times(component)
    println("[METRICS]: total time for component " + component + ": " + total)
  }
}