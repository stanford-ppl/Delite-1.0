package ppl.apps.ml.baseline.lbp


object BPNode {
  val CAT_BELIEF_SIZE = 8
  val REL_BELIEF_SIZE = 6
  val CAT = BPNodeType.CAT
  val REL = BPNodeType.REL
}

class BPNode {
    var valid = false
    var ntype: BPNodeType = null
    var potential: Array[Double] = null
    var belief: Array[Double] = null
    var exact_belief: Array[Double] = null
    var belief_size: Int = 0
    var edges: Array[BPNode] = null
    var num_edges: Int = 0
    var messages: Array[Array[Double]] = null
    var rel_node_edge_cntr: Int = 0
    var real_belief: Int = 0

    var active: Int = 0

    var message_id: Array[Int] = null /*xli*/
}