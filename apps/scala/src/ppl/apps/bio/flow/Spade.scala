package ppl.apps.bio.flow

import ppl.delite.core.{Delite, DeliteApplication}
import ppl.delite.dsl.optiml.{Matrix, Vector}
import ppl.delite.core.appinclude._
import ppl.delite.dsl.primitive.DeliteInt

import org.rosuda.JRI.Rengine
import org.rosuda.JRI.REXP
import org.rosuda.JRI.RMainLoopCallbacks
import org.rosuda.JRI.RConsoleOutputStream
import ppl.delite.dsl.optiml.io.{MLInputReader, MLOutputWriter}
import ppl.delite.dsl.optiml.train.TrainingSet
import ppl.delite.metrics.PerformanceTimer

object Spade extends DeliteApplication {
  def print_usage = {
    println("Usage: Downsampling <input data file> <output data file>")
    exit(-1)
  }

  def run(args: Array[String]) = {

    /*
    println("Creating Rengine ")
    val re = new Rengine()

    System.out.println("Rengine created, waiting for R");
		// the engine creates R is a new thread, so we should wait until it's ready
    if (!re.waitForR()) {
        System.out.println("Cannot load R");
    }

    println(re.eval("3+5"))
    System.out.println("Now the console is yours ... have fun");
    re.startMainLoop();
    */


    if (args.length < 2) print_usage

    Delite.init = true

    val arcsinh_cofactor:Double = 5
    val downsampling_scaling_factor:Double = 5
    val used_markers = Vector[Int](0)
    val is_normalize:Boolean = false
    val normalize_weight_factor:Double = 1

    Delite.init = false

    /********* downsampling *********/

    val densities = Downsampling.get_downsampling_info (args(0), arcsinh_cofactor, downsampling_scaling_factor,
                                           used_markers, is_normalize, normalize_weight_factor)
    for(i <- 0 to 9)
      print(densities(i) + " ")
    println()
    /*
    if (MLOutputWriter.write(Matrix(densities),args(1))==false)
      println("Failed to write output to file")
    else
      println("Downsampling finished")
    */

    /********* clustering *********/
    /*
    val data = TrainingSet(MLInputReader.read(args(0) + "/data.txt"))
    PerformanceTimer.start("clustering")
    val assgn = Clustering.cluster(data, 200)
    PerformanceTimer.stop("clustering")
    PerformanceTimer.print("clustering")

    for(i <- 0 to 9)
      print(assgn(i)+" ")
    print("\n")
    */
    
    /********* upsampling *********/
    /*
    val tbl = MLInputReader.read(args(0) + "/tbl.txt")
    val cluster_data = MLInputReader.read(args(0) + "/cluster_data.txt")
    val cluster_assign = MLInputReader.read(args(0) + "/cluster_assign.txt").mapRowsToVec(_(0).toInt)
    cluster_assign.force

    PerformanceTimer.start("assign_cluster")
    val assign = Upsampling.assign_cluster(tbl, cluster_data, cluster_assign)
    PerformanceTimer.stop("assign_cluster")
    PerformanceTimer.print("assign_cluster")

    for(i <- 0 to 9)
      print(assign(i)+" ")
    print("\n")
    */
  }

}