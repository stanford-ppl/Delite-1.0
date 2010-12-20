package ppl.delite.metrics

import collection.mutable.{HashMap, ArrayBuffer}
import java.io.{BufferedWriter, File, PrintWriter, FileWriter}
import ppl.delite.core.Config

/**
 * User: Anand Atreya
 */

object PerformanceTimerAggregate
{
  private val currentTimer = new HashMap[String, Long]
  private val times = new HashMap[String, ArrayBuffer[Double]]

  private var currentEpoch = 0

  // Call incrementEpoch every time you want to start a new timing batch, such as for multiple runs of an app
  def incrementEpoch = {
    times.foreach { pair =>
      pair._2 += 0.0
    }
    currentEpoch += 1
  }

  def start(component: String) {
    if (!times.contains(component)) {
      times += component -> ArrayBuffer[Double](0.0)
    }
    currentTimer += component -> System.nanoTime
  }

  def stop(component: String) {
    val x = (System.nanoTime - currentTimer(component)) / 1000000000.0
    times(component)(currentEpoch) += x
  }

  def print(component: String) {
    try {
      val timeString = times(component).last formatted ("%.6f") + "s"
      println("[METRICS]: Current aggregate time for component " + component + ": " + timeString)
    }
    catch {
      case e: Exception => println("[METRICS]: No data for component " + component)
    }
  }

  def save(component: String) {
    try {
      val procstring = {
        if (Config.bypassExecutor && Config.debugEnabled) {
        // set procs to -1 for bypassExecutor case
          "-1"
        } else {
          Config.CPUThreadNum.toString
        }
      }
      var file: String = new File("./").getCanonicalPath + "/" + component + ".p" + procstring + ".times"
      val writer = new PrintWriter(new BufferedWriter(new FileWriter(file, false)))
      times(component).foreach { time =>
        writer.println(time formatted ("%.10f"))
      }
      writer.close()
    }
    catch {
      case e: Exception => println("[METRICS]: Unable to save data for component " + component)
    }
  }
}