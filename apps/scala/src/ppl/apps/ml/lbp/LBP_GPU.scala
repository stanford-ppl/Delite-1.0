package ppl.apps.ml.lbp

/* Entry point for Loopy Belief Propagation application.
 * Adapted from Cearch-bp code by Donald Yeung, 17 Apr 05
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Jul 23, 2009
 * modified: Jul 23, 2009
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import ppl.delite.dsl.optiml.{Vector, Matrix}
import ppl.delite.metrics._
import ppl.delite.core.{Delite, DeliteCore, DeliteApplication}

object LBP_GPU extends DeliteApplication {
   def print_usage = {
    println("Usage: LBP_GPU <edge file> <print file>")
    println("Example: LBP_GPU onlyedges graphprint")
    exit(-1)
  }

  def run(args: Array[String]) = {
    if (args.length < 2) print_usage
    val edges = args(0)
    val print = args(1)

    val num = 10
    for (i <- 0 until num) {
      Delite.init = true
      val model = new LBPModel()
      model.load(edges, print)
      Delite.init = false

      PerformanceTimer.start("LBPGPU")
      model.train

      PerformanceTimer.stop("LBPGPU")
      PerformanceTimer.print("LBPGPU")
    }

    PerformanceTimer.save("LBPGPU")
  }
  
}
