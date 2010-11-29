package ppl.apps.bio.flow

import ppl.delite.core.{Delite, DeliteApplication}
import ppl.delite.dsl.optiml.io.MLOutputWriter
import ppl.delite.dsl.optiml.{Matrix, Vector}
import ppl.delite.core.appinclude._
import ppl.delite.dsl.primitive.DeliteInt

object Spade extends DeliteApplication {
  def print_usage = {
    println("Usage: Downsampling <input data file> <output data file>")
    exit(-1)
  }

  def run(args: Array[String]) = {
    if (args.length < 2) print_usage

    Delite.init = true

    val arcsinh_cofactor:Double = 5
    val downsampling_scaling_factor:Double = 5
    val used_markers = Vector[Int](2)
    used_markers(0) = 0
    used_markers(1) = 1
    // used_markers.median[DeliteInt]
    val is_normalize:Boolean = false
    val normalize_weight_factor:Double = 1

    Delite.init = false

    val densities = Downsampling.get_downsampling_info (args(0), arcsinh_cofactor, downsampling_scaling_factor,
                                           used_markers, is_normalize, normalize_weight_factor)

    if (MLOutputWriter.write(Matrix(densities),args(1))==false)
      println("Failed to write output to file")
    else
      println("Downsampling finished")
  }

}