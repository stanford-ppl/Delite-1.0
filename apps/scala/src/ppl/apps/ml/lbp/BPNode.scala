package ppl.apps.ml.lbp


import ppl.delite.dsl.optiml.{Matrix, Vector}

/* Description
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Jul 26, 2009
 * modified: Jul 26, 2009
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object BPNode {
  val CAT_BELIEF_SIZE = 8
  val REL_BELIEF_SIZE = 6
  val CAT = BPNodeType.CAT
  val REL = BPNodeType.REL
}
class BPNode extends Node {
    var valid = false
    var ntype: BPNodeType = null
    var potential: Vector[Double] = null
    var belief: Vector[Double] = null
    var exact_belief: Vector[Double] = null
    var belief_size: Int = 0
    var edges: Vector[BPNode] = null
    var num_edges: Int = 0
    var messages: Matrix[Double] = null
    var rel_node_edge_cntr: Int = 0
    var real_belief: Int = 0

    var active: Int = 0

    var message_id: Vector[Int] = null /*xli*/
}