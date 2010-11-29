package ppl.apps.bh

import ppl.delite.core.DeliteApplication

object BarnesHut extends DeliteApplication {

  def run(args: Array[String]) {

    //trcLogger.debug("[Barnes and Hut] starts")

    // load the universe
    Loader.open(args(0))

    val N = Loader.getNumBodies
    val radius = Loader.getSize

    // read in and initialize bodies
    val bodies = new Array[Body](N)
    for (i <- 0 until N) {
      bodies(i) = Loader.body()
      println("Body " + bodies(i))
    }

    Loader.close()

    NBodyBHSim.setParams(N, radius, bodies)
    NBodyBHSim.run()

  }

} // object BarnesHut
