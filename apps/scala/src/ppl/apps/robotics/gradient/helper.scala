package ppl.apps.robotics.gradient

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._

/**
 * Created by IntelliJ IDEA.
 * User: Anand Atreya
 * Date: Oct 17, 2010
 * Time: 9:25:19 PM
 * To change this template use File | Settings | File Templates.
 */

object helper {

  /**
   * Determines if two rectangles intersect
   * @param a
   * @param b
   * @return boolean value indicating if the two rectangles intersect
   */
  def intersect(a: Rect, b: Rect): Boolean = {
    ((a.x < (b.x + b.width)) && ((a.x + a.width) > b.x) && ((a.y + a.height) > b.y) && (a.y < (b.y + b.height)))
  }

  /**
   * Computes the fraction of the intersection of two rectangles with respect to the
   * total area of the rectangles.
   * @param a
   * @param b
   * @return intersection area
   */
  def rectFractOverlap(a: Rect, b: Rect): Float = {
    if (intersect(a, b)) {
      val total_area: Float = b.height * b.width + a.width * a.height
      val left = if (a.x > b.x) a.x else b.x
      val top = if (a.y > b.y) a.y else b.y
      val right = if (a.x + a.width < b.x + b.width) a.x + a.width else b.x + b.width
      val width = right - left
      val bottom = if (a.y + a.height < b.y + b.height) a.y + a.height else b.y + b.height
      val height = bottom - top
      2.0f * height * width / (total_area + 0.000001f) //Return the fraction of intersection
    } else {
      0.0f
    }
  }

  /**
   * Suppress overlapping rectangle to be the rectangle with the highest score
   * @param detections vector of detections to work with
   * @param frac_overlap what fraction of overlap between 2 rectangles constitutes overlap
   */
  def nonMaxSuppress(detections: Vector[BiGGDetection], frac_overlap: Float): Vector[BiGGDetection] = {
    var len = detections.length

    var i = 0
    while (i < len - 1) {
      var j = i + 1
      var iMoved = false
      while (j < len && iMoved == false) {
        val measured_frac_overlap = rectFractOverlap(detections(i).roi, detections(j).roi)
        if (measured_frac_overlap > frac_overlap) {
          if (detections(i).score >= detections(j).score) {
            val temp = detections(len - 1)
            detections(len - 1) = detections(j)
            detections(j) = temp
            len -= 1
            j -= 1
          }
          else {
            val temp = detections(len - 1)
            detections(len - 1) = detections(i)
            detections(i) = temp
            len -= 1
            i -= 1
            iMoved = true
          }
        }
        j += 1
      }
      i += 1
    }
    detections.take(len)
  }
}