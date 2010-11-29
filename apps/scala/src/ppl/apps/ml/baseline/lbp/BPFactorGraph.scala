package ppl.apps.ml.baseline.lbp

class BPFactorGraph {
  import BPNode._
  import LBP._

  var nodes: Array[BPNode] = null

  def construct(max_node_num: Int, edges: Array[Array[Int]]) : Int = {
    nodes = new Array[BPNode](max_node_num+1)
    for (i <- 0 until nodes.length){
      nodes(i) = new BPNode()
      nodes(i).valid = false
    }

    var max_cat_node = 0
    var num_edges = 0
    for (i <- 0 until edges.length) {
      if (!(nodes(edges(i)(0)).valid)) {
        /* initialize category node */
        nodes(edges(i)(0)).valid = true
        nodes(edges(i)(0)).ntype = BPNode.CAT;
        nodes(edges(i)(0)).belief = new Array(CAT_BELIEF_SIZE)
        nodes(edges(i)(0)).potential = new Array[Double](CAT_BELIEF_SIZE)
        nodes(edges(i)(0)).belief_size = CAT_BELIEF_SIZE;
        nodes(edges(i)(0)).num_edges = 0
        num_edges = 0
        var j = i
        while ((j < edges.length) && (edges(j)(0) == edges(i)(0))) {
          num_edges += 1
          j += 1
        }
        nodes(edges(i)(0)).edges = new Array[BPNode](num_edges).map(n => new BPNode()) //(node **)malloc(num_edges*sizeof(node *));
        nodes(edges(i)(0)).messages = Array.ofDim[Double](num_edges, CAT_BELIEF_SIZE) //(double **)malloc(num_edges*sizeof(double *));
        //for (j <- 0 until num_edges){
        //  nodes(edges(i)(0)).messages(j) = new Vector[Double](CAT_BELIEF_SIZE) //(double *)malloc(CAT_BELIEF_SIZE*sizeof(double));
        //}
        nodes(edges(i)(0)).rel_node_edge_cntr = 0
        if (edges(i)(0) > max_cat_node){
          max_cat_node = edges(i)(0)
        }

        nodes(edges(i)(0)).message_id = new Array[Int](num_edges) //(int *)malloc(num_edges*sizeof(int)); /*xli  local message_id in the connected node*/
      }

      if (!(nodes(edges(i)(1)).valid)) {
        /* initialize relation node */
        nodes(edges(i)(1)).valid = true
        nodes(edges(i)(1)).ntype = BPNode.REL
        nodes(edges(i)(1)).belief = new Array[Double](REL_BELIEF_SIZE)
    //(double *)malloc(REL_BELIEF_SIZE*sizeof(double));
        nodes(edges(i)(1)).potential = new Array[Double](REL_BELIEF_SIZE)
    //(double *)malloc(REL_BELIEF_SIZE*sizeof(double));
        nodes(edges(i)(1)).belief_size = REL_BELIEF_SIZE
        nodes(edges(i)(1)).num_edges = 0
        nodes(edges(i)(1)).edges = new Array[BPNode](2).map(n => new BPNode()) //(node **)malloc(2*sizeof(node *));
        nodes(edges(i)(1)).messages = Array.ofDim[Double](2, REL_BELIEF_SIZE) //(double **)malloc(2*sizeof(double *));
        //for (j <- 0 until 2){
        //  nodes(edges(i)(1)).messages(j) = new Array[Double](REL_BELIEF_SIZE) //(double *)malloc(REL_BELIEF_SIZE*sizeof(double));
        //}
        nodes(edges(i)(1)).rel_node_edge_cntr = 0

        nodes(edges(i)(1)).message_id = new Array[Int](2) //(int *)malloc(2*sizeof(int));

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
