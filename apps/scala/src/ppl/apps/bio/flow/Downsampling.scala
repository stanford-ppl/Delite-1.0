package ppl.apps.bio.flow

/* Port of parfor_1_read_individiual_fcs_get_downsampling_info.m
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Aug 3, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

import ppl.delite.dsl.optiml._
import io.{MLOutputWriter, MLInputReader}
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.core.appinclude._
import ppl.delite.metrics.PerformanceTimer
import specialized.{DoubleMatrix, StreamingDoubleMatrixImpl}
import train.TrainingSet
import ppl.delite.core.{DeliteApplication, Delite}
import ppl.delite.dsl.primitive.{DeliteInt, DeliteDouble}

object Downsampling{

  def get_downsampling_info(filename: String, arcsinh_cofactor: Double, downsampling_scaling_factor: Double,
                            used_markers: Vector[Int], is_normalize: Boolean, normalize_weight_factor: Double)
                            : Vector[Int] = {

    val data = TrainingSet( MLInputReader.read(filename) )

    println("   Input matrix size: " + data.numRows + "*" + data.numCols)

    // TODO: we need to push this implementation detail out of the application and into the DSL
    // (1) number of cells if data matrix is small, (2) block size = 2000 if data large, (3) if number of cells is
    //  extremely large, we need to limit block size, otherwise line 38 (in matlab code) will be out of memory
    val numSamples = min(data.numRows, 2000, Math.floor(2500000000l/data.numRows)).intValue
    println("   numSamples = " + numSamples)

    PerformanceTimer.start("compute_median_min_dist")
    val med_min_dist = compute_median_min_dist(data, numSamples)
    PerformanceTimer.stop("compute_median_min_dist")
    println("   med_min_dist = " + med_min_dist)

    val kernel_width = downsampling_scaling_factor * med_min_dist
    val apprx_width  = 1.5 * med_min_dist
    println("   For this " + data.numRows + " channel data, KERNEL WIDTH is " + kernel_width + ", APPRX WIDTH is " + apprx_width)

    PerformanceTimer.start("count_neighbors")
    val densities = count_neighbors(data, kernel_width, apprx_width)
    PerformanceTimer.stop("count_neighbors")

    PerformanceTimer.print("compute_median_min_dist")
    PerformanceTimer.print("count_neighbors")

    densities
  }

  def compute_median_min_dist(data: TrainingSet[Double], numSamples: Int, target_prctile: Int = 5, kernel_width_para: Int = 10) = {
    println("   finding empirical dist of the min distance between cells ...")

    // 2 inefficiencies here:
    //   1) should push sampling into the parallel loop
    //   2) calculating the nearest neighbor implies calculating its distance, so doing it again is redundant

    // sampled_data is numSamples x data.numCols
    val index = Vector.mrange(0, data.numRows)
    val sampled_index = sample(index, numSamples, "random")

    val min_dist = (0::numSamples) { i =>
      val sample_idx = sampled_index(i)
      val neighbor_idx = nearest_neighbor(sample_idx, data)
      val d = data.dist(sample_idx, neighbor_idx)
      d
    }

    val median = min_dist.median

    median
  }

  def count_neighbors(data: TrainingSet[Double], kernel_width: Double, apprx_width: Double) : Vector[Int] = {

    println("   finding local density for each cell ...")

    val distances = Matrix[Double](data.numRows, data.numRows, (i,j)=> data.dist(i,j))

    //TODO streaming version (fastest streaming version)
    val densities = Vector[Int](data.numRows)
    distances.foreachRow { (row, idx) =>
      if(idx%1000 == 0) println("  (streaming) # processed node = " + idx)
      if(densities(idx) == 0) {
        val distances_in_range = row map (e=> if (e < kernel_width) 1 else 0)  // find neighbors in range
        val c = distances_in_range.sum[DeliteInt]    // count # of neighbors
        val neighbors = row find (_ < apprx_width)   // find approx neighbors
        densities.updatev(neighbors,c)               // approximately update
      }
    }
    
/*
    //TODO streaming row version (using find)
    val densities = Vector[Int](data.numRows)
    distances.foreachRow { row =>
      if(row.index%1000 == 0) println("  (streaming row find) # processed node = " + row.index)
      if(densities(row.index) == 0) {
        val neighbors = row find (_ < kernel_width)
        val apprxs = row find (_ < apprx_width)
        densities.updatev(apprxs,neighbors.length)
      }
    }
*/
/*
    //TODO streaming row version (using row.idx, slow because of force)
    val densities = Vector[Int](data.numRows)
    distances.foreachRow { row =>
      if(row.index%1000 == 0) println("  (streaming row) # processed node = " + row.index)
      if(densities(row.index) == 0) {
        val distances_in_range = row map (e=> if (e < kernel_width) 1 else 0)  // find neighbors in range
        val c = distances_in_range.sum[DeliteInt]    // count # of neighbors
        val neighbors = row find (_ < apprx_width)   // find approx neighbors
        densities.updatev(neighbors,c)               // approximately update
      }
    }
*/
/*
    //TODO fused streaming version (using nRow and Array) (fastest fused version)
    val nRows = data.numRows
    val densities = Vector[Int](nRows)
    distances.foreachRow { (row, idx) =>
      if(idx%1000 == 0) println("  (streaming fusing - nRows, Array) # processed node = " + idx)
      if(densities(idx) == 0) {
        var c = 0
        var j = 0
        var neighbors = new Array[Int](nRows)
        var neighbor_id = 0
        while(j < nRows){
          val e = data.dist(idx, j)
          val r = if (e < kernel_width) 1 else 0
          c += r
          if(e < apprx_width){
            neighbors(neighbor_id) = j
            neighbor_id += 1
          }
          j += 1
        }
        while(neighbor_id > 0){
          neighbor_id -= 1
          densities(neighbors(neighbor_id)) = c
        }
      }
    }
*/
/*
    //TODO fused streaming version (using row.length and Vector)
    val densities = Vector[Int](data.numRows)
    distances.foreachRow { (row, idx) =>
      if(idx%1000 == 0) println("  (streaming fusing - row.length, Vector) # processed node = " + idx)
      if(densities(idx) == 0) {
        var c = 0
        var j = 0
        var neighbors = Vector[Int](0)
        while(j < row.length){
          val e = data.dist(idx, j)
          val r = if (e < kernel_width) 1 else 0
          c += r
          if (e < apprx_width)  neighbors += j
          j += 1
        }
        densities.updatev(neighbors,c)
     }
    }
*/
/*
    //TODO fused streaming version (using nRows and Vector)
    val nRows = data.numRows
    val densities = Vector[Int](nRows)
    distances.foreachRow { (row, idx) =>
      if(idx%1000 == 0) println("  (streaming fusing - nRows, Vector) # processed node = " + idx)
      if(densities(idx) == 0) {
        var c = 0
        var j = 0
        var neighbors = Vector[Int](0)
        while(j < nRows){
          val e = data.dist(idx, j)
          val r = if (e < kernel_width) 1 else 0
          c += r
          if (e < apprx_width)  neighbors += j
          j += 1
        }
        densities.updatev(neighbors,c)
     }
    }
*/
/*
    //TODO fused streaming version (using nRows, Array and row.index)
    val nRows = data.numRows
    val densities = Vector[Int](nRows)
    distances.foreachRow { row =>
      if(row.index%1000 == 0) println("  (streaming fusing - nRows, Array, row.index) # processed node = " + row.index)
      if(densities(row.index) == 0) {
        var c = 0
        var j = 0
        var neighbors = new Array[Int](nRows)
        var neighbor_id = 0
        while(j < nRows){
          val e = data.dist(row.index, j)
          val r = if (e < kernel_width) 1 else 0
          c += r
          if(e < apprx_width){
            neighbors(neighbor_id) = j
            neighbor_id += 1
          }
          j += 1
        }
        while(neighbor_id > 0){
          neighbor_id -= 1
          densities(neighbors(neighbor_id)) = c
        }
      }
    }
*/
/*
    //TODO fused streaming version (using data.numRows, Array)
    val densities = Vector[Int](data.numRows)
    distances.foreachRow { (row, idx) =>
      if(idx%1000 == 0) println("  (streaming fusing - data.numRows, Array) # processed node = " + idx)
      if(densities(idx) == 0) {
        var c = 0
        var j = 0
        var neighbors = new Array[Int](data.numRows)
        var neighbor_id = 0
        while(j < data.numRows){
          val e = data.dist(idx, j)
          val r = if (e < kernel_width) 1 else 0
          c += r
          if(e < apprx_width){
            neighbors(neighbor_id) = j
            neighbor_id += 1
          }
          j += 1
        }
        while(neighbor_id > 0){
          neighbor_id -= 1
          densities(neighbors(neighbor_id)) = c
        }
      }
    }
*/

/*
    //TODO buffered version (deprecated)
    val densities = Vector[Int](data.numRows)
    (0::data.numRows).mforeach { idx =>
      if(idx%1000 == 0) println("  (buffered) # processed node = " + idx)
      if(densities(idx) == 0) {
        val row = distances(idx)
        val distances_in_range = row map (e=> if (e < kernel_width) 1 else 0)
        val c = distances_in_range.sum[DeliteInt]
        val neighbors = row find (_ < apprx_width)
        // densities(neighbors) = c
        densities.updatev(neighbors,c)
      }
    }().force
*/
/*
    //TODO imperative version 1: parallel outer loop, w/ approximation (fastest imperative version)
    val nRows = data.numRows
    var densities = Vector[Int](nRows)
    (0::nRows).mforeach { i =>
      if(i%1000 == 0) println("  (imperative 1) # processed node = " + i)
      if(densities(i) <= 0) {
        var c = 0
        var j = 0
        val apprxs = new Array[Int](nRows)
        var appr_idx = 0
        while( j < nRows) {
          val d = data.dist(i, j)
          if(d < kernel_width) c = c + 1
          if(d < apprx_width) {
            apprxs(appr_idx) = j
            appr_idx += 1
          }
          j += 1
        }
        while( appr_idx > 0){
          appr_idx -= 1
          val neighbor = apprxs(appr_idx)
          if(densities(neighbor)==0) densities(neighbor) = c
        }
      }
    }().force
*/
/*
    //TODO imperative version 2: sequential outer loop, w/ approximation
    var densities = Vector[Int](data.numRows)
    for (i <- 0 until data.numRows) {
      if(i%100 == 0) println("   (imperative 2) # processed node = " + i)

      if(densities(i) <= 0){
        val c = sumd(0, data.numRows){ j =>
          val d = dist(data(i), data(j))
          if (d < kernel_width) 1 else 0
        }

        densities = (0::data.numRows) { j =>
          val d = dist(data(i), data(j))
          if (d < apprx_width) {
            c.asInstanceOf[Int]
          }
          else {
            densities(j)
          }
        }
        densities(i) = c.asInstanceOf[Int]
      }
    }
*/
    densities
  }

}

