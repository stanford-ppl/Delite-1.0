/* VectorView must be backed by an array reference. It can be used to perform
 * vector operations (e.g. map) on a strided sequence of data (e.g., a row of
 * a row-major matrix).              
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 17, 2010
 * modified: Feb 17, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.dsl.optiml

import ppl.delite.core.appinclude._
import scala.reflect.ClassManifest

trait VectorView[T] extends Vector[T] {
  def start: Int
  def stride: Int

  protected var _start: Int
  protected var _stride: Int

  ////////////
  // data ops

  def idx(n: Int) = _start + n*_stride

  def indices : Vector[Int] = {
    Vector.range(0, length).map(e => e*_stride)
  }

  def +=[A <: T](x: A) = clone.+=(x)
  def copyFrom[A <: T](pos: Int, xs: Vector[A]) = clone.copyFrom(pos,xs)
  def insert[A <: T](pos: Int, x: A) = clone.insert(pos,x)
  def insertAll[A <: T](pos: Int, xs: Vector[A]) = clone.insertAll(pos,xs)
  def removeAll(pos: Int, len: Int) = clone.removeAll(pos,len)
  def trim = clone.trim
}
