package ppl.apps.bh

import ppl.delite.statistics.Animation

abstract class NBodySim {
  protected var t = 0.0 // simulation time
  protected val dt = 0.1 // time quantum
  protected var N: Int = _
  protected var R: Double = _
  protected var B: Array[Body] = _
  protected var U: Quad = _

  def setParams(n: Int, r: Double, b: Array[Body]) {
    N = n
    R = r
    B = b
    U = new Quad(0.0, 0.0, 0.0, R)
  }

} // class NBodySim

object NBodyBruteSim extends NBodySim {
  def run() {
    // simulate the universe
    while (true) {
      // update the forces
      for (i <- 0 until N) {
        B(i).resetForce()
        for (j <- 0 until N) {
          if (i != j) B(i).addForce(B(j))
        }
      }
      // update the bodies
      for (i <- 0 until N) {
        B(i).update(dt)
      }
      // draw the bodies
      Animation.clear(Animation.BLACK)
      for (i <- 0 until N) {
        B(i).draw()
      }
      Animation.show(10)
    }
  }

} // object NBodyBruteSim

object NBodyBHSim extends NBodySim {

  def run() {
    // turn on animation mode and rescale coordinate system
    Animation.init("Barnes-Hut Animation")
    Animation.show(0)
    Animation.setXscale(-R, R)
    Animation.setYscale(-R, R)

    // simulate the universe
//    while (true) {
    for (it <- 1 to 100) {
      var updates = 0
      val framestart = System.currentTimeMillis
      // reconstruct the tree
      val root = new BHTree(U)
      for (b <- B) root.insert(b)
      val tct = (System.currentTimeMillis - framestart) / 1000.0
      Animation.clear(Animation.BLACK)
      //println(root)
      root.draw()
      val ups = System.currentTimeMillis
      // update the forces
      for (b <- B) {
        b.resetForce()
        updates += root.updateForce(b)
      }
      val upt = (System.currentTimeMillis - ups) / 1000.0
      // update the bodies
      for (b <- B) {
        b.update(dt)
        b.draw()
      }
      val spf = (System.currentTimeMillis - framestart) / 1000.0
      val txb = "FPS: " + ((1.0 / spf) formatted ("%6.1f")) +
              "  FRAME: " + (it formatted ("%5d")) + " : " + (spf formatted ("%7.1E")) + "s"
      val txt = "BODIES: " + (B.size formatted ("%5d")) +
                " (" + BHTree.density + ") UPDATES: " + (updates formatted ("%5d")) +
                "  TREE: " + (tct formatted ("%7.1E")) + "s" +
                "  UPDT: " + (upt formatted ("%7.1E")) + "s"
      Animation.setPenColor(Animation.GREEN)
      Animation.text(0.0,  1.05 * R , txt)
      Animation.text(0.0, -1.05 * R , txb)
      Animation.show(10)
    }
  }

} // object NBodyBruteSim
