package ppl.apps.bh

import ppl.delite.statistics.Animation

class Quad(val xmid: Double, val ymid: Double, val zmid: Double, val length: Double) {

  def contains(x: Double, y: Double, z: Double) = {
    xmid - length <= x && x < xmid + length &&
    ymid - length <= y && y < ymid + length &&
    zmid - length <= z && z < zmid + length
  }

  def getLength() = 2 * length

  def getCell(index: Int) = {
    val l = length / 2.0
    val x = if ((index & 1) > 0) l else -l
    val y = if ((index & 2) > 0) l else -l
    val z = if ((index & 4) > 0) l else -l
    new Quad(xmid + x, ymid + y, zmid + z, l)
  }

  def draw() {
    Animation.setPenColor(Animation.WHITE)
    Animation.square(xmid, ymid, length)
  }

  private def f(v: Double) = (v formatted ("%8.1E"))

  override def toString = ("<" + f(xmid - length) + ":" + f(xmid + length) + ", "
                               + f(ymid - length) + ":" + f(ymid + length) + ", "
                               + f(zmid - length) + ":" + f(zmid + length) + ">")

} // class Quad
