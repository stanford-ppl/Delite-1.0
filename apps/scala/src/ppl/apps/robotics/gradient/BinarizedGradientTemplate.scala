package ppl.apps.robotics.gradient

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._

/**
 * Created by IntelliJ IDEA.
 * User: Anand Atreya
 * Date: Oct 17, 2010
 * Time: 10:36:45 PM
 * To change this template use File | Settings | File Templates.
 */

class BinarizedGradientTemplate {
  /**
   * IDdentiy of the object this template belongs to
   */
  //	int id;

  /**
   * In the reduced image. The side of the template square is then 2*r+1.
   */
  var radius = 0

  /**
   * Holds a tighter bounding box of the object in the original image scale
   */
  var rect: Rect = null
  var mask_list = Vector[Int]()

  /**
   * Pyramid level of the template (reduction_factor = 2^level)
   */
  var level = 0

  /**
   * The list of gradients in the template
   */
  var binary_gradients: Array[Int] = null

  /**
   * indices to use for matching (skips zeros inside binary_gradients)
   */
  var match_list: Vector[Int] = null

  /**
   * This is a match list of list of sub-parts. Currently unused.
   */
  var occlusions: Vector[Vector[Int]] = null

  var templates: Vector[BinarizedGradientTemplate] = null

  var hist: Vector[Float] = null

  /**
   * Score this template against another template
   * @param test the other template
   * @param match_thresh allow for early exit if you are not above this
   * @return the score
   */
  def score(test: BinarizedGradientTemplate, match_thresh: Float, match_table: Matrix[Int], match_method: Int): Float = {
    if (radius != test.radius) {
//      println(radius + " != " + test.radius)
      return -1.0f
    }
    var total: Float = match_list.length.asInstanceOf[Float]
    if (total == 0.0) {
      return -1.0f
    }
    val num_test: Float = test.match_list.length.asInstanceOf[Float]
    if ((num_test / total) < match_thresh) {
      return num_test / total; //Not enough entries in the other list to be above match_thresh
    }
    var matches: Float = 0;
    if (match_method == 0) {
      var limit = (total * (1.0 - match_thresh) + 0.5).asInstanceOf[Int] //Miss more than this number and we can't be above match_thresh
      for (i <- 0 until match_list.length) {
        if (binary_gradients(i) == 0 && test.binary_gradients(i) == 0) {
          matches += 1
        }
        else if (((binary_gradients(i)) & (test.binary_gradients(i))) > 0) {
          matches += 1
        }
        else {
          limit -= 1
          if (limit <= 0) {
            return (match_thresh - 0.000001f) //sunk below the limit of misses, early terminate
          }
        }
      }
    } else { //match_method == 1, so we use the cosine matching table
      val limit = (match_thresh * 100.0).asInstanceOf[Int] //Since the matchtable are unsigned chars going from 0 to 100;
      var max_score = 100 * total
      for (i <- 0 until match_list.length) {
        var res = match_table(binary_gradients(i), test.binary_gradients(i))
        matches += res
        max_score -= (100 - res) //Remember, we coded perfect match to be 100.
        if (max_score < limit)
          return (match_thresh - 0.0001f)
      }
      matches /= 100 //Since we were 100x too high
    }
    return (matches / total).asInstanceOf[Float]
  }
}