package ppl.apps.robotics.gradientsimplified

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._

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