package ppl.apps.bh.dsl

import ppl.delite.core.include._
import ppl.delite.core.DeliteDSLType

object Quad {

  def apply(xmid: Double, ymid: Double, zmid: Double, length: Double)
           (implicit fact: Factory, strat: Quad.Strategy) = fact(xmid, ymid, zmid, length)(strat)

  class Proxy extends Quad {
    override def nodeAttr = "[shape=box]"
    override def toString = "\"Quad" + super.toString

    def factory: Quad.Factory = force.factory
    def strategy: Quad.Strategy = force.strategy
  }

  trait Factory {
    def newQuad(xmid: Double, ymid: Double, zmid: Double, length: Double)
               (implicit strat: Quad.Strategy): Quad

    def apply(xmid: Double, ymid: Double, zmid: Double, length: Double)
             (implicit strat: Quad.Strategy): Quad = newQuad(xmid, ymid, zmid, length)(strat)
  } // trait Factory

  trait Strategy

} // object Quad

trait Quad extends DeliteDSLType {
  import Quad._

  type DSLType = Quad

  def factory: Quad.Factory
  def strategy: Quad.Strategy
  def byteSize: Long = throw new UnsupportedOperationException
/*
  def contains(x: Double, y: Double, z: Double) = {
    xmid - length <= x && x < xmid + length &&
    ymid - length <= y && y < ymid + length &&
    zmid - length <= z && z < zmid + length
  }
*/
//  def getLength() = 2 * length
/*
  def getCell(index: Int) = {
    val l = length / 2.0
    val x = if ((index & 1) > 0) l else -l
    val y = if ((index & 2) > 0) l else -l
    val z = if ((index & 4) > 0) l else -l
    factory(xmid + x, ymid + y, zmid + z, l)(strategy)
  }

  def draw() {
    Animation.setPenColor(Animation.WHITE)
    Animation.square(xmid, ymid, length)
  }
*/
  private def f(v: Double) = (v formatted ("%8.1E"))
/*
  override def toString = ("<" + f(xmid - length) + ":" + f(xmid + length) + ", "
                               + f(ymid - length) + ":" + f(ymid + length) + ", "
                               + f(zmid - length) + ":" + f(zmid + length) + ">")
*/
} // trait Quad
