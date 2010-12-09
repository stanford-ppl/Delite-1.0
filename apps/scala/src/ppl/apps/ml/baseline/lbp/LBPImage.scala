package ppl.apps.ml.baseline.lbp

import util.Random

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 12/08/2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

class LBPImage(val rows: Int, val cols: Int) {
  val data = Array.fill(rows * cols)(0.0)
  val pixels = rows * cols

  def vertid(i: Int, j: Int) = {
    assert(i < rows)
    assert(j < cols)

    i * cols + j
  }

  def paintSunset(numRings: Int) = {
    val centerR = rows / 2.0
    val centerC = cols / 2.0
    val maxRadius = Math.min(rows, cols) / 2.0

    for(r <- 0 until rows) {
      for(c <- 0 until cols) {
        val distance = Math.sqrt((r-centerR)*(r-centerR) + (c-centerC)*(c-centerC))

        // If on top of image
        if(r < rows / 2) {
          // Compute ring of sunset
          val ring = Math.floor(Math.min(1.0, distance/maxRadius) * (numRings - 1))

          data(vertid(r,c)) = ring
        }
        else {
          data(vertid(r,c)) = 0
        }
      }
    }
  }

  // Corrupt the image with Gaussian noise
  def corrupt(sigma: Double) = {
    for(i <- 0 until(rows * cols, 2)) {
      data(i) += Random.nextGaussian * sigma
    }
  }
}