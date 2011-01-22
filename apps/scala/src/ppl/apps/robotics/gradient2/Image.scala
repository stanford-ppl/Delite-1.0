package ppl.apps.robotics.gradient2

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._

class Image[T] extends Matrix[T] {
  def downsample(rowFactor: Int, colFactor: Int)(block: Matrix[T] => T): Image[T] = {
    val output = Matrix[T](data.numRows / rowFactor, data.numCols / colFactor)
    var row = 0
    while (row < output.numRows) {
      var col = 0
      while (col < output.numCols) {
        output(row, col) = block(data.slice2d(rowFactor * row, rowFactor * row + rowFactor, colFactor * col, colFactor * col + colFactor))
        col += 1
      }
      row += 1
    }
    new Image[T](output)
  }

  def windowedFilter(sliceRows: Int, sliceCols: Int)(block: Matrix[T] => T) : Image[T] = {
    // Need to enforce odd values for sliceRows and sliceCols
    val rowOffset = (sliceRows - 1) / 2
    val colOffset = (sliceCols - 1) / 2
    val output = Matrix[T](data.numRows, data.numCols)
    var row = rowOffset
    while (row < data.numRows - rowOffset) {
      var col = colOffset
      while (col < data.numCols - colOffset) {
        output(row, col) = block(data.slice2d(row - rowOffset, row + rowOffset, col - colOffset, col + colOffset))
        col += 1
      }
      row += 1
    }
    new Image[T](output)
  }
}
