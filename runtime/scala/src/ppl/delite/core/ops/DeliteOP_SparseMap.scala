package ppl.delite.core.ops

import ppl.delite.core.{DeliteDSLType, DeliteUnit, DeliteFunc, DeliteCollection}
import ppl.delite.dsl.optiml.{SparseMatrix, ArithOps, appinclude}
import ppl.delite.dsl.optiml.appinclude

/**
 * Created by IntelliJ IDEA.
 * User: joe
 * Date: Aug 3, 2010
 * Time: 10:17:37 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class DeliteOP_SparseMap[A,B,C[X] <: DeliteCollection[X]](implicit ops: ArithOps[A]) extends DeliteOP[C[B]] {

  val coll: SparseMatrix[A]

  val out: C[B]

  def func: A => B

  final override def isChunkable = true

  override def getImmutableDeps = {
    val immDeps = if (coll != out) Seq(coll) else Seq()
    func match {
      case f: DeliteFunc => immDeps ++ f.deps
      case _ => immDeps
    }
  }

  override def getMutableDeps: Seq[DeliteDSLType] = Seq(out)

  protected var total = 1

  @volatile
  protected var chunkDone: Array[Boolean] = null

  override def submitChunks(num_chunks: Int, submit: DeliteOP[DeliteUnit] => Unit) {
    chunkDone = new Array[Boolean](num_chunks-1)
    total = num_chunks
    var i = 1
    while (i != num_chunks) {
      submit(new SlaveChunk(i, num_chunks))
      i += 1
    }
  }

def task = {
    val chunk_size = coll.numRows / total
    val remainder = coll.numRows % total
    val end = if (remainder != 0) chunk_size + 1 else chunk_size
    var row = 0 //what row we are at
    var outI = 0 //index of what position we are in the "out" collection
//    println("in main task, row = 0, and end = " + end)

    while(row != end){ //Iterate through each row of the SM

        if(coll.row_idx_apply(row) == -1){ //this row is empty
          var columnI = 0                 //map everything with 0 values
          while(columnI < coll.numCols){
            out.dc_update(outI, func(ops.zero))
            outI += 1
            columnI += 1
          }
        }
        else{ //this row isn't empty
          //first, find the next non-empty row
          var nextRow = row + 1
          while(coll.row_idx_apply(nextRow) == -1) nextRow += 1
          //now, get that row's column index (so we know when to stop)
          val toStop = coll.row_idx_apply(nextRow)
          var idxA = coll.row_idx_apply(row)

          var columnI = 0 //iterate through this row's columns

          while(idxA < toStop){ //for each NNZ element in this row....
            //iterate through zero values before the next NNZ value
            while(columnI != coll.col_idx_apply(idxA)){
              out.dc_update(outI, func(ops.zero))
              outI += 1
              columnI += 1
            }
            //now, map the NNZ value
            out.dc_update(outI, func(coll.dc_apply(idxA)))
            columnI += 1
            outI += 1
            idxA += 1
          }
          //now, finish off mapping any remaining zero values in this row
          while(columnI < coll.numCols){
            out.dc_update(outI, func(ops.zero))
            outI += 1
            columnI += 1
          }

        } //end mapping this (non-empty) row

        row += 1
    }


    //barrier
    var ci = 0
    val cend = if (chunkDone != null) chunkDone.length else 0
    while (ci != cend) {
      while (!chunkDone(ci)) { }
      ci += 1
    }
    out
  }

  private class SlaveChunk(pos: Int, total: Int) extends DeliteOP[DeliteUnit] {

    override def getImmutableDeps = null
    override def getMutableDeps = null

    def task = {
      val chunk_size = coll.numRows / total
      val remainder = coll.numRows % total
      val sz = if (pos < remainder) chunk_size + 1 else chunk_size
      val off = if (pos < remainder) 0 else pos - remainder
      
      var row = pos*(chunk_size+1) - off
      val end = row + sz
      var outI = row*coll.numCols
//      println("in slave chunk, row = " + row + " end = " + end)

      while (row != end) {
        if(coll.row_idx_apply(row) == -1){ //this row is empty

          var columnI = 0
          while(columnI < coll.numCols){ //TODO: is this really faster than a for loop?
            out.dc_update(outI, func(ops.zero))
            outI += 1
            columnI += 1
          }
        }
        else{ //this row isn't empty
          //first, find the next non-empty row
          var nextRow = row + 1
          while(coll.row_idx_apply(nextRow) == -1) nextRow += 1
          //now, get that row's column index (so we know when to stop)
          val toStop = coll.row_idx_apply(nextRow)
          var idxA = coll.row_idx_apply(row)

          var columnI = 0 //iterate through this row's columns
          while(idxA < toStop){
            //iterate through zero values before the next NNZ value
            while(columnI != coll.col_idx_apply(idxA)){
              out.dc_update(outI, func(ops.zero))
              outI += 1
              columnI += 1
            }
            //now, map the NNZ value
            out.dc_update(outI, func(coll.dc_apply(idxA)))
            columnI += 1
            outI += 1
            idxA += 1
          }
          //now, finish off any remaining zero values in this row
          while(columnI < coll.numCols){
            out.dc_update(outI, func(ops.zero))
            outI += 1
            columnI += 1
          }

        } //end iteration through this (non-empty) row

        row += 1
      }
      chunkDone(pos-1) = true
    }
  }


}