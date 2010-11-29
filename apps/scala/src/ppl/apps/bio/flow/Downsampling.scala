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
import train.TrainingSet
import ppl.delite.core.{DeliteApplication, Delite}
import ppl.delite.dsl.primitive.{DeliteInt, DeliteDouble}

object Downsampling{

  def get_downsampling_info(filename: String, arcsinh_cofactor: Double, downsampling_scaling_factor: Double,
                            used_markers: Vector[Int], is_normalize: Boolean, normalize_weight_factor: Double)
                            : Vector[Int] = {

//    val data = TrainingSet( MLInputReader.read(filename) )

    val input = Matrix[Double](10,3)
    input(0,0) = 0
    input(0,1) = 1
    input(0,2) = 2
    input(1,0) = 3
    input(1,1) = 4
    input(1,2) = 5
    input(2,0) = 6
    input(2,1) = 7
    input(2,2) = 8
    input(3,0) = 9
    input(3,1) = 10
    input(3,2) = 11
    input(4,0) = 12
    input(4,1) = 13
    input(4,2) = 14
    input(5,0) = 15
    input(5,1) = 16
    input(5,2) = 17
    input(6,0) = 18
    input(6,1) = 19
    input(6,2) = 20
    input(7,0) = 21
    input(7,1) = 22
    input(7,2) = 23
    input(8,0) = 24
    input(8,1) = 25
    input(8,2) = 26
    input(9,0) = 27
    input(9,1) = 28
    input(9,2) = 29
    val data = TrainingSet(input)

    println("   Input matrix size: " + data.numRows + "*" + data.numCols)

    // TODO: we need to push this implementation detail out of the application and into the DSL
    // (1) number of cells if data matrix is small, (2) block size = 2000 if data large, (3) if number of cells is
    //  extremely large, we need to limit block size, otherwise line 38 (in matlab code) will be out of memory
    val numSamples = min(data.numRows, 2000, Math.floor(2500000000l/data.numRows)).intValue
    println("   numSamples = " + numSamples)

    val med_min_dist = compute_median_min_dist(data, numSamples)
    println("   med_min_dist = " + med_min_dist)

    val kernel_width = downsampling_scaling_factor * med_min_dist
    val apprx_width  = 1.5 * med_min_dist
    println("   For this " + data.numRows + " channel data, KERNEL WIDTH is " + kernel_width + ", APPRX WIDTH is " + apprx_width)

    val densities = count_neighbors(data, kernel_width, apprx_width)

    densities
  }

  def compute_median_min_dist(data: TrainingSet[Double], numSamples: Int, target_prctile: Int = 5, kernel_width_para: Int = 10) = {
    println("   finding empirical dist of the min distance between cells ...")

    // 2 inefficiencies here:
    //   1) should push sampling into the parallel loop
    //   2) calculating the nearest neighbor implies calculating its distance, so doing it again is redundant
    
    // sampled_data is numSamples x data.numCols
    val sampled_data = sample(data, numSamples)

    val min_dist = (0::numSamples) { i =>
      val sample = sampled_data(i)
      val neighbor = nearest_neighbor(sample, data)
      val d = dist(sample, neighbor)
      d
    }

    min_dist.median
  }

  def count_neighbors(data: TrainingSet[Double], kernel_width: Double, apprx_width: Double) : Vector[Int] = {

    println("   finding local density for each cell ...")
/*
    val distances = distm(data, data)
    val distances_in_range = distances map (x => if (x < kernel_width) 1 else 0)
    val densities_true = distances_in_range.mapRowsToVec( row => row.sum[DeliteInt].value )
    val neighbors = distances map (x => if (x < apprx_width) true else false)
    var densities = Vector[Int](data.numRows)
*/
/*
    /**
    *  TODO functional sequential, no race
    */
    val indices = (0::densities_true.length)

    // this is a list of indices (lists) that each element of the density vector would set with no conflicts
    val indicesInOrderOfUpdate = indices.map( i => indices.filter(neighbors(i,_)))
    println("\nindicesInOrderOfUpdate = ")
    for (i <- 0 until indicesInOrderOfUpdate.length){
      indicesInOrderOfUpdate(i).pprint
    }

    val indicesDeduplicated = setzeros(indicesInOrderOfUpdate)
    println("\nindicesDeduplicated = ")
    for (i <- 0 until indicesDeduplicated.length){
      indicesDeduplicated(i).pprint
    }
    
    // generate a cumulative list of the indices updated from left to right
    val indicesCumulativeUpdated = indices map ( i => (Vector.flatten(indicesDeduplicated.slice(0,i))).distinct )
    println("\nindicesCumulativeUpdated = ")
    for (i <- 0 until indicesCumulativeUpdated.length){
      indicesCumulativeUpdated(i).pprint
    }
    
    // filter the original list to leave only unique updates
    // e.g. uniqueUpdates = { (0,1), (), (2,3), (), (4,5), (), (6,7), (), (8,9) }
    val uniqueUpdates = indices.map( i => indicesDeduplicated(i).filter(!indicesCumulativeUpdated(i).contains(_)))
    println("\nuniqueUpdates = ")
    for (i <- 0 until uniqueUpdates.length){
      uniqueUpdates(i).pprint
    }

    // retrieve the final densities result, guaranteed to be disjoint due to uniqueness (but not known statically..)
    // indices.foreach { i => densities(uniqueUpdates(i)) = densities_true(i)}
    indices.foreach { i => uniqueUpdates(i).foreach { j => densities(j) = densities_true(i) }}

    println("\ndensities = ")
    densities.pprint
*/
/*
    /**
    * TODO imperative sequential, no race
    */
    for (i <- 0 until densities.length) {
      if (densities(i) == 0) {
        val indices = (0::densities.length).filter(neighbors(i,_))
        // out_densities(indices) = in_densities(i)
        // the above is shorthand for:
        for (j <- 0 until indices.length) {
          if(densities(indices(j))==0)
            densities(indices(j)) = densities_true(i)
        }
      }
    }
*/
/*
    /**
    * TODO parallel, intentional race
    */
    (0::densities.length).mforeach {i =>
      if (densities(i) == 0) { // race!
        val indices = (0::densities.length).filter(neighbors(i,_))
        // out_densities(indices) = in_densities(_)
        // the above is shorthand for:
        for (j <- 0 until indices.length) {
          if(densities(indices(j))==0)
            densities(indices(j)) = densities_true(i)
        }
      }
    }().force
*/
/*
    /**
    * TODO sequential, no race, using flag
    */
    
    val distances = distm(data, data)
    val distances_in_range = distances filter (_ < kernel_width)
    var densities = distances_in_range.map(x => if (x == true) 1 else 0).mapRowsToVec( row => row.sum[DeliteInt].value )
    val neighbors = distances filter (_ < apprx_width)

    val flag = Vector[Boolean](neighbors.numRows).map(e => true)
    val out = densities.clone
    for(i <- 0 until neighbors.numRows){
      if(flag(i))
        for(j <- 0 until neighbors.numCols){
          if(neighbors(i,j)){
            if(flag(j)) out(j) = densities(i)
            flag(j) = false
          }
        }
    }
    densities = out
*/

    //TODO version 1: parallel outer loop, w/o approximation
    var densities = Vector[Int](data.numRows)
    (0::data.numRows).mforeach { i =>
      if(i%100 == 0) println("   # processed node = " + i)

      val c = sumd(0, data.numRows){ j =>
        val d = dist(data(i), data(j))
        if (d < kernel_width) 1 else 0
      }
      densities(i) = c.asInstanceOf[Int]
    }().force

/*
    //TODO version 2: parallel outer loop, w/ approximation
    var densities = Vector[Int](data.numRows)
    (0::data.numRows).mforeach { i =>
      if(i%100 == 0) println("   # processed node = " + i)

      if(densities(i) <= 0) {
        var c = 0
        val dist_i = (0::data.numRows) { j =>
          val d = dist(data(i), data(j))
          if(d < kernel_width) c = c + 1
          d
        }
        (0::data.numRows){ j=>
          if (dist_i(j) < apprx_width && densities(j) <= 0)
            densities(j) = c
        }
      }
      else {
        densities(i)
      }
    }().force
*/
/*
    //TODO version 3: sequential outer loop, w/o approximation
    var densities = Vector[Int](data.numRows)
    for (i <- 0 until data.numRows) {
      if(i%100 == 0) println("   # processed node = " + i)

      val c = sumd(0, data.numRows){ j =>
        val d = dist(data(i), data(j))
        if (d < kernel_width) 1 else 0
      }
      densities(i) = c.asInstanceOf[Int]
    }
*/
/*
    //TODO version 4: sequential outer loop, w/ approximation
    var densities = Vector[Int](data.numRows)
    for (i <- 0 until data.numRows) {
      if(i%100 == 0) println("   # processed node = " + i)

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

  def setzeros(vv: Vector[Vector[Int]]): Vector[Vector[Int]] = {
    val out = Vector[Vector[Int]](vv.length)
    for(i <- 0 until vv.length)
      out(i) = vv(i).clone
    for(i <- 0 until vv.length)
      for(j <- 0 until out(i).length)
        if(out(i)(j) > i){
          // println("[" + i + "] set out(" + out(i)(j) + ") = null")
          out(out(i)(j)) = Vector[Int](0)
        }
    out
  }
  
}

