package ppl.apps.robotics.gradientsimplified

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._

object BinarizedGradientTemplate {

  /**
   * Score this template against another template
   * test: the other template
   * match_thresh: allow for early exit if you are not above this
   * returns: the score
   */
  def score(test1: BinarizedGradientTemplate, test2: BinarizedGradientTemplate, match_thresh: Float): Float = {
    if (test1.radius != test2.radius) {
      return -1.0f
    }
    var total: Float = test1.match_list.length.asInstanceOf[Float]
    if (total == 0.0) {
      return -1.0f
    }
    val num_test: Float = test2.match_list.length.asInstanceOf[Float]
    if ((num_test / total) < match_thresh) {
      return num_test / total //Not enough entries in the other list to be above match_thresh
    }
    var matches: Float = 0
    var limit = (total * (1.0 - match_thresh) + 0.5).asInstanceOf[Int] //Miss more than this number and we can't be above match_thresh
    for (i <- 0 until test1.match_list.length) {
      if (test1.binary_gradients(test1.match_list(i)) == 0 && test2.binary_gradients(test1.match_list(i)) == 0) {
        matches += 1
      }
      else if (((test1.binary_gradients(test1.match_list(i))) & (test2.binary_gradients(test1.match_list(i)))) > 0) {
        matches += 1
      }
      else {
        limit -= 1
        if (limit <= 0) {
          return (match_thresh - 0.000001f) //sunk below the limit of misses, early terminate
        }
      }
    }
    return (matches / total).asInstanceOf[Float]
  }
}

class BinarizedGradientTemplate (
  // In the reduced image. The side of the template square is then 2*r+1.
  val radius: Int,

  // Holds a tighter bounding box of the object in the original image scale
  val rect: Rect,
  val mask_list: Vector[Int],

  // Pyramid level of the template (reduction_factor = 2^level)
  val level: Int,

  // The list of gradients in the template
  val binary_gradients: Vector[Int],

  // indices to use for matching (skips zeros inside binary_gradients)
  val match_list: Vector[Int],

  // This is a match list of list of sub-parts. Currently unused.
  val occlusions: Vector[Vector[Int]],

  val templates: Vector[BinarizedGradientTemplate],

  val hist: Vector[Float]) {
}

