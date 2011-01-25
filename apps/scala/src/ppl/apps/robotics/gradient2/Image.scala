package ppl.apps.robotics.gradient2

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._

class Image[T] extends Matrix[T] {
  def downsample(rowFactor: Int, colFactor: Int)(block: Matrix[T] => T): Image[T] = {
    (0 :: numRows / rowFactor, 0 :: numCols / colFactor) { (row, col) =>
      block(slice2d(rowFactor * row, rowFactor * row + rowFactor, colFactor * col, colFactor * col + colFactor))
    }
  }

  def windowedFilter[B](sliceRows: Int, sliceCols: Int)(block: Matrix[T] => B) : Image[B] = {
    // Need to enforce odd values for sliceRows and sliceCols
    val rowOffset = (sliceRows - 1) / 2
    val colOffset = (sliceCols - 1) / 2
    (0 :: numRows, 0 :: numCols) { (row, col) =>
      if ((row >= rowOffset) && (row < numRows - rowOffset) && (col >= colOffset) && (col < numCols - colOffset)) {
        block(slice2d(row - rowOffset, row + rowOffset + 1, col - colOffset, col + colOffset + 1))
      } else {
        0.asInstanceOf[B]
      }
    }
  }
}
