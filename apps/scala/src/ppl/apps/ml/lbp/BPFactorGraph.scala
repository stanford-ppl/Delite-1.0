package ppl.apps.ml.lbp

/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Jul 26, 2009
 * modified: Jul 26, 2009
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml.{Vector, Matrix}

object BPFactorGraph {
  implicit val bpVecPFact = vecProxyFactory[BPNode]
}

class BPFactorGraph extends FactorGraph {
  import BPFactorGraph._
  import BPNode._
  import LBP._
  
  var nodes: Vector[BPNode] = null

  def construct(max_node_num: Int, edges: Matrix[Int]) : Int = {
    nodes = Vector[BPNode](max_node_num+1)
    for (i <- 0 until nodes.length){
      nodes(i) = new BPNode()
      nodes(i).valid = false
    }

    var max_cat_node = 0
    var num_edges = 0
    for (i <- 0 until edges.numRows) {
      if (!(nodes(edges(i)(0)).valid)) {
        /* initialize category node */
        nodes(edges(i)(0)).valid = true
        nodes(edges(i)(0)).ntype = BPNode.CAT;
        nodes(edges(i)(0)).belief = Vector[Double](CAT_BELIEF_SIZE)
        nodes(edges(i)(0)).potential = Vector[Double](CAT_BELIEF_SIZE)
        nodes(edges(i)(0)).belief_size = CAT_BELIEF_SIZE;
        nodes(edges(i)(0)).num_edges = 0
        num_edges = 0
        var j = i
        while ((j < edges.numRows) && (edges(j)(0) == edges(i)(0))) {
          num_edges += 1
          j += 1
        }
        nodes(edges(i)(0)).edges = Vector[BPNode](num_edges).map(n => new BPNode()) //(node **)malloc(num_edges*sizeof(node *));
        nodes(edges(i)(0)).messages = Matrix[Double](num_edges, CAT_BELIEF_SIZE) //(double **)malloc(num_edges*sizeof(double *));
        //for (j <- 0 until num_edges){
        //  nodes(edges(i)(0)).messages(j) = new Vector[Double](CAT_BELIEF_SIZE) //(double *)malloc(CAT_BELIEF_SIZE*sizeof(double));
        //}
        nodes(edges(i)(0)).rel_node_edge_cntr = 0
        if (edges(i)(0) > max_cat_node){
          max_cat_node = edges(i)(0)
        }

        nodes(edges(i)(0)).message_id = Vector[Int](num_edges) //(int *)malloc(num_edges*sizeof(int)); /*xli  local message_id in the connected node*/
      }

      if (!(nodes(edges(i)(1)).valid)) {
        /* initialize relation node */
        nodes(edges(i)(1)).valid = true
        nodes(edges(i)(1)).ntype = BPNode.REL
        nodes(edges(i)(1)).belief = Vector[Double](REL_BELIEF_SIZE)
    //(double *)malloc(REL_BELIEF_SIZE*sizeof(double));
        nodes(edges(i)(1)).potential = Vector[Double](REL_BELIEF_SIZE)
    //(double *)malloc(REL_BELIEF_SIZE*sizeof(double));
        nodes(edges(i)(1)).belief_size = REL_BELIEF_SIZE
        nodes(edges(i)(1)).num_edges = 0
        nodes(edges(i)(1)).edges = Vector[BPNode](2).map(n => new BPNode()) //(node **)malloc(2*sizeof(node *));
        nodes(edges(i)(1)).messages = Matrix[Double](2, REL_BELIEF_SIZE) //(double **)malloc(2*sizeof(double *));
        //for (j <- 0 until 2){
        //  nodes(edges(i)(1)).messages(j) = new Array[Double](REL_BELIEF_SIZE) //(double *)malloc(REL_BELIEF_SIZE*sizeof(double));
        //}
        nodes(edges(i)(1)).rel_node_edge_cntr = 0

        nodes(edges(i)(1)).message_id = Vector[Int](2) //(int *)malloc(2*sizeof(int));

      }

      /* fill message_id of the node in the other side */
      nodes(edges(i)(0)).message_id(nodes(edges(i)(0)).num_edges) = nodes(edges(i)(1)).num_edges
      nodes(edges(i)(1)).message_id(nodes(edges(i)(1)).num_edges) = nodes(edges(i)(0)).num_edges

      /* enter the edge */
      nodes(edges(i)(0)).edges(nodes(edges(i)(0)).num_edges) = (nodes(edges(i)(1)))
      nodes(edges(i)(0)).num_edges += 1
      nodes(edges(i)(1)).edges(nodes(edges(i)(1)).num_edges) = (nodes(edges(i)(0)))
      nodes(edges(i)(1)).num_edges += 1

    }

    /* fill in the unconnected nodes */
    for (i <- 0 until nodes.length) {
      if (!(nodes(i).valid)) {
        nodes(i).valid = true
        nodes(i).ntype = if (i <= max_cat_node) BPNode.CAT else BPNode.REL
        nodes(i).belief = null
        nodes(i).belief_size = 0
        nodes(i).edges = null
        nodes(i).num_edges = 0
        nodes(i).messages = null
        nodes(i).message_id= null
      }
    }

    return max_cat_node    
  }
}