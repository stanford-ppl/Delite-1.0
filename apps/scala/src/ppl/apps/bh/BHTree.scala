package ppl.apps.bh

class BHTree(val quad: Quad) {
  private var branch: List[BHTree] = List()
  private var body: List[Body] = List()
  private var population = 0
  private var updates = 0

  def insert(bo: Body) {
    if (population < BHTree.density) {
      body = bo :: body
      population += 1
    } else {
      val bodies = body
      body = List((bo /: body)(_ + _))
      if (branch.isEmpty) {
        for (indx <- 0 to 7) {
          val qd = quad.getCell(indx)
          val node = new BHTree(qd)
          for (ob <- bodies; if (ob in qd)) node insert ob
          if (bo in qd) node insert bo
          branch = node :: branch
        }
      } else (branch find (bo in _.quad)) match {
        case Some(node) => node insert bo
        case None => // error
      }
    }
  }

  /*
   1. If the current node is an external node (and it is not body b),
      calculate the force exerted by the current node on b,
      and add this amount to b's net force.
   2. Otherwise, calculate the ratio s / d. If s / d < ?,
      treat this internal node as a single body, and calculate the force
      it exerts on body b, and add this amount to b's net force.
   3. Otherwise, run the procedure recursively on each of the current node's children.
  */
  def updateForce(bo: Body): Int = {
    var updates = 0
    if (!body.isEmpty) {
      if (branch.isEmpty) for (b <- body; if bo != b) {
        bo.addForce(b)
        updates += 1
      }
      else {
        val b = body.head
        val d = bo.distanceTo(b)
        val s = quad.getLength()
        if (s < d * BHTree.theta) { bo.addForce(b); updates += 1 }
        else for (node <- branch) updates += node.updateForce(bo)
      }
    }
    updates
  }

  def getUpdates() = updates

  def draw() {
    quad.draw()
    for (node <- branch; if !node.body.isEmpty) node.draw()
  }

  override def toString = {
    val indent = BHTree.indent
    var output = indent + "{" + quad.toString + " <" + body.toString + ">"
    if (!branch.isEmpty) {
      output += "\n"
      BHTree.indent += "   "
      for (node <- branch; if !node.body.isEmpty) output += node.toString + "\n"
      BHTree.indent = indent
      output += indent
    }
    output += "}"
    output
  }

} // class BHTree

object BHTree {
  val density = 5 // maximum number of bodies before cell split
  val theta = 0.5 // accuracy parameter: > cell side / distance
  var indent = ""
}
