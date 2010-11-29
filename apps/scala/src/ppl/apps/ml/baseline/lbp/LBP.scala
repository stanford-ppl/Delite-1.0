package ppl.apps.ml.baseline.lbp

import ppl.delite.metrics.PerformanceTimer

object LBP {
   def print_usage = {
    println("Usage: LBP <edge file> <print file>")
    println("Example: LBP onlyedges graphprint")
    exit(-1)
  }

  def main(args: Array[String]) = {
    if (args.length < 2) print_usage
    val edges = args(0)
    val print = args(1)

    val num = 10
    for (i <- 0 until num) {

      val model = new LBPModel()
      model.load(edges, print)


      PerformanceTimer.start("LBPbaseline")
      model.train

      PerformanceTimer.stop("LBPbaseline")
      PerformanceTimer.print("LBPbaseline")
    }

    PerformanceTimer.save("LBPbaseline")
  }

}