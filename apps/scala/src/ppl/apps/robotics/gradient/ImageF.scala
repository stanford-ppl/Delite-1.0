package ppl.apps.robotics.gradient

import ppl.delite.dsl.optiml._

/**
 * Created by IntelliJ IDEA.
 * User: Anand Atreya
 * Date: Oct 11, 2010
 * Time: 1:13:22 AM
 * To change this template use File | Settings | File Templates.
 */

// single-channel floating point image
class ImageF(val rows: Int, val cols: Int, val data: Matrix[Float]) {
  def this(rows: Int, cols: Int) = {
    this (rows, cols, Matrix[Float](rows, cols))
  }

  def this(data: Matrix[Float]) = {
    this (data.numRows, data.numCols, data)
  }

  def getRow(row: Int) = {
    data.getRow(row)
  }
}