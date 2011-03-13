package ppl.apps.bio.flow

import ppl.delite.dsl.optiml.train.TrainingSet
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml._
import ppl.delite.metrics.PerformanceTimer

/**
 * Author: Bo Wang
 * Date: Jan 6, 2011
 * Time: 11:05:01 AM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object Upsampling {

  def assign_cluster(data: Matrix[Double],       // testing data-set
                     cluster_data: Matrix[Double],    // cluster coordinate
                     cluster_assign: Vector[Int]) = { // cluster to be assigned

    println("obs = "+data.numRows+", dim = "+data.numCols+", cls = "+cluster_data.numRows)
/*
    // TODO: imperative version
    var assign = Vector[Int](data.numRows)
    val numCluster = cluster_data.numRows
    (0::data.numRows).mforeach{ idx =>
      if(idx%1000 == 0) println("  (imperative) # processed node = " + idx)
      var min_idx  = 0
      var min_dist = Math.MAX_DOUBLE
      var j = 0
      while(j < numCluster){
        // val d = dist(row,cluster_data(j))
        val d = dist(data, idx, cluster_data, j)
        if (d < min_dist) {
          min_idx = j
          min_dist = d
        }
        j += 1
      }
      assign(idx) = cluster_assign(min_idx)
    }().force
*/
/*
    // TODO: concise, most direct experssion, slowest version
    val assign = data.mapRowsToVec { row =>
      val distances = cluster_data.mapRowsToVec(dist(row,_))
      val min_idx = distances.minIndex
      cluster_assign(min_idx)
    }.force
*/

    // TODO: concise, dist(Vector, Matrix)
    val assign = data.mapRowsToVec { row =>
      val distances = dist(row, cluster_data)
      val min_idx = distances.minIndex
      cluster_assign(min_idx)
    }.force

/*
    // TODO: concise, dist(Vector, Matrix), VectorView -> Vector
    val assign = data.mapRowsToVec { row_view =>
      val row:Vector[Double] = row_view.map(e=>e)
      val distances = dist(row, cluster_data)
      val min_idx = distances.minIndex
      cluster_assign(min_idx)
    }.force
*/
/*
    // TODO: concise, fusing
    val assign = data.mapRowsToVec { row_view =>
      val row:Vector[Double] = row_view.map(e=>e)
      val v_data = get_data(row)
      val m_data = get_data(cluster_data)
      val numRows = cluster_data._numRows
      val numCols = cluster_data._numCols
      var min_idx = -1
      var smallest = Math.MAX_DOUBLE
      var r = 0
      while(r < numRows){
        var offset_i = 0
        var offset_j = r*numCols
        var sum:Double = 0
        while(offset_i < numCols){
          val tmp = v_data(offset_i) - m_data(offset_j)
          sum += java.lang.Double.longBitsToDouble((java.lang.Double.doubleToRawLongBits(tmp)<<1)>>>1)
          offset_i += 1
          offset_j += 1
        }
        if(sum < smallest){
          smallest = sum
          min_idx = r
        }
        r += 1
      }
      cluster_assign(min_idx)
    }.force
*/

    assign.asInstanceOf[Vector[Int]]
  }

}
