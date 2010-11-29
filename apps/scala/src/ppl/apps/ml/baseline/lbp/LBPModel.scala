package ppl.apps.ml.baseline.lbp

import java.io.File

object LBPModel {
  private val HUGE_VALUE = 1e200
  private val MIN_PREC = 1e-100
  private val MAX_ITER = 50
  private val CONVERGE_THRESH = 0.001
}

class LBPModel {
  import LBPModel._
  import LBPParser._
  import LBP._

  private var _graph : BPFactorGraph = null
  private var _edge_potential : Array[Double] = new Array[Double](BPNode.CAT_BELIEF_SIZE*BPNode.REL_BELIEF_SIZE)

  private var max_cat_node = 0
  private var max_node_num = 0

  private var last_fraction_not_converged = 0.0
  private var last_avg_dist = 0.0
  private var last_max_dist = 999.0

  private var _lbp_iter = 0

  def load(efile: String, bfile : String) {
    /* first pass for edges */
    var inFile = new File(efile)
    val (num_nodes, num_edges, max_line_size, _max_node_num) = get_graph_values(inFile)
    max_node_num = _max_node_num

    printf("num nodes:  %d\n", num_nodes);
    printf("num edges:  %d\n", num_edges);
    printf("max line:  %d\n", max_line_size);
    printf("max node:  %d\n", max_node_num);

    /* init variables for second pass */
    inFile = new File(efile)
    val (read_edges, node_ids, edges) = read_graph_lr(inFile, max_node_num, num_edges)
    printf("Read %d edges from graph\n", read_edges);

    /* create graph from edges */
    _graph = new BPFactorGraph()
    max_cat_node = _graph.construct(max_node_num, edges)
    printf("create graph for %d nodes\n",max_node_num);

     /* read beliefs */
    inFile = new File(bfile)
    _edge_potential = read_beliefs(inFile, _graph.nodes, max_cat_node, max_node_num);


  }


  def train() {
    val last_cat_node = max_cat_node
    val last_node = max_node_num
    val nodes = _graph.nodes

    var i = 0
    while (i < MAX_ITER && last_max_dist > CONVERGE_THRESH) {
      //calc_messages(nodes, Edges, nodeIdx, mssgIdx, edges_last_cat)
      //calc_beliefs(nodes, Edges, nodeIdx, mssgIdx, last_node)

      calculate_messages(last_cat_node, nodes)
      calculate_beliefs(last_node, nodes)

      print_state
      i += 1
    }
    println("Finished in " + i + " iters")

    //var outFile = new File("pages.cat")
    //print_beliefs(outFile, 0, last_cat_node, nodes)
    //outFile = new File("rels.rel_type")
    //print_beliefs(outFile, last_cat_node+1, last_node, nodes)
  }



  def sum_out_product(potential: Array[Double], that_belief: Array[Double], that_message: Array[Double],
      that_size: Int, this_size: Int, first: Boolean) : Array[Double] = {

    val this_mult = if (first) 6 else 1
    val that_mult = if (first) 1 else 6


    var min = HUGE_VALUE
    for (i <- 0 until that_size)
      if ((that_message(i) > 0) && (that_message(i) < min))
        min = that_message(i)

    if (min < MIN_PREC) min = MIN_PREC

    val vals = new Array[Double](this_size)
    var sum = 0.0
    for (i <- 0 until this_size) {
      var num = 0.0
      var denum = 0.0
      var entry = 0.0
      var v = 0.0
      for (j <- 0 until that_size) {
        num = that_belief(j)
        if (num > 0) {
          denum = that_message(j)
          if (denum > MIN_PREC) {
            entry = num / (denum / min);
            v += entry * _edge_potential(i*this_mult + j*that_mult);
          }
        }
      }
      vals(i) = v;
      sum += v;
     //v
    }

    for (i <- 0 until this_size) {
      vals(i) /= sum;
    }
    vals
  }


  def calculate_messages(last_cat_node : Int, nodes : Array[BPNode]) {

    for (i <- 0 to last_cat_node) {
      if ((nodes(i).ntype == BPNodeType.CAT) && (nodes(i).valid)) {
        for (j <- 0 until nodes(i).num_edges) {
          val to = nodes(i).edges(j)
          val to_message_id = nodes(i).message_id(j)

          val answer1 = sum_out_product(_edge_potential, to.belief, to.messages(to_message_id),
                          BPNode.REL_BELIEF_SIZE, BPNode.CAT_BELIEF_SIZE, true);
          val answer2 = sum_out_product(_edge_potential, nodes(i).belief, nodes(i).messages(j),
                          BPNode.CAT_BELIEF_SIZE,  BPNode.REL_BELIEF_SIZE, false);

          nodes(i).messages(j) = answer1
          to.messages(to_message_id) = answer2
        }
      }
    }
  }



  def calculate_beliefs(last_node: Int, nodes: Array[BPNode]) {

    var max = 0.0
    var dist_sum = 0.0
    var num_not_converged = 0

    val distances = new Array[Double](last_node+1)
    for (i <- 0 to last_node) {
      var distance = 0.0
      if (nodes(i).valid) {
        /* make a copy of the current belief */
        val tmp_belief = new Array[Double](BPNode.CAT_BELIEF_SIZE)
        for (j <- 0 until nodes(i).belief_size) {
          tmp_belief(j) = nodes(i).belief(j)
          nodes(i).belief(j) = nodes(i).potential(j)
        }

        /* integrate messages into belief */
        for (j <- 0 until nodes(i).num_edges) {
          var sum = 0.0
          for (k <- 0 until nodes(i).belief_size) {
            nodes(i).belief(k) *= nodes(i).messages(j)(k)
            sum += nodes(i).belief(k)
          }
          for (k <- 0 until nodes(i).belief_size)
            nodes(i).belief(k) /= sum;
        }

        /* compute distance between new and previous beliefs */
        for (j <- 0 until nodes(i).belief_size) {
          var cur = Math.abs(nodes(i).belief(j) - tmp_belief(j))
          if (cur > distance)
            distance = cur
        }
      }
      distances(i) = distance
    }

    //NOTE: calling .length should force distances to be ready
    for (i <- 0 until distances.length) {
      val distance = distances(i)
      if (distance > max)
        max = distance
      if (distance > CONVERGE_THRESH) {
        num_not_converged += 1
        dist_sum += distance
      }
    }

    last_fraction_not_converged = num_not_converged.toDouble / (last_node - 38)
    last_avg_dist = if (num_not_converged == 0) 0 else dist_sum / num_not_converged
    last_max_dist = max
  }

  private var lbp_iter = 0
  def print_state {
    println("Iter " + lbp_iter + ": fraction not converged: " + last_fraction_not_converged + " max dist: " + last_max_dist + " avg dist: " + last_avg_dist)
    lbp_iter += 1
  }

}