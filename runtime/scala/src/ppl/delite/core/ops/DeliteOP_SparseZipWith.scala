package ppl.delite.core.ops

import ppl.delite.core.{DeliteUnit, DeliteFunc, DeliteCollection}
import ppl.delite.dsl.optiml._

/**
 * Created by IntelliJ IDEA.
 * User: naranb
 * Date: Jul 23, 2010
 * Time: 5:34:17 PM
 * To change this template use File | Settings | File Templates.
 */
/*
main issue: Sparse matrix can't be represented as a collection
how to generalize this without specifically referring to sparse matrix?

  */
abstract class DeliteOP_SparseZipWith[A,B,R,C[X] <: DeliteCollection[X]] (implicit ops: ArithOps[A]) extends DeliteOP[C[R]] {
  val collA: SparseMatrix[A]
  val collB: DeliteCollection[B]
  val out: C[R]

  protected var total = 1

  @volatile
  protected var chunkDone: Array[Boolean] = null

  final override def isChunkable = true

  def func: (A, B) => R

  override def getImmutableDeps = {
    val immDeps = if (collA != out && collB != out) Seq(collA, collB)
      else if (collA != out) Seq(collA)
      else if (collB != out) Seq(collB)
      else Seq()

    func match {
      case f: DeliteFunc => immDeps ++ f.deps
      case _ => immDeps
    }
  }

  override def getMutableDeps = Seq(out)

  override def submitChunks(num_chunks: Int, submit: DeliteOP[DeliteUnit] => Unit) {
    chunkDone = new Array[Boolean](num_chunks-1)
    total = num_chunks
    var i = 1
    while (i != num_chunks) {
      submit(new SlaveChunk(collA, collB, out, i, num_chunks))
      i += 1
    }
  }

  def task = {
    val chunk_size = collA.numRows / total
    val remainder = collA.numRows % total
    val end = if (remainder != 0) chunk_size + 1 else chunk_size
    var row = 0 //what row we are at
    var outI = 0 //index of what position we are in the "out" collection
//    println("in main task, row = 0, and end = " + end)

    while(row != end){ //Iterate through each row of the SM

        if(collA.row_idx_apply(row) == -1){ //this row is empty
          var columnI = 0                 //map everything with 0 values
          while(columnI < collA.numCols){
            out.dc_update(outI, func(ops.zero, collB.dc_apply(outI)))
            outI += 1
            columnI += 1
          }
        }
        else{ //this row isn't empty
          //first, find the next non-empty row
          var nextRow = row + 1
          while(collA.row_idx_apply(nextRow) == -1) nextRow += 1
          //now, get that row's column index (so we know when to stop)
          val toStop = collA.row_idx_apply(nextRow)
          var idxA = collA.row_idx_apply(row)

          var columnI = 0 //iterate through this row's columns

          while(idxA < toStop){ //for each NNZ element in this row....
            //iterate through zero values before the next NNZ value
            while(columnI != collA.col_idx_apply(idxA)){
              out.dc_update(outI, func(ops.zero, collB.dc_apply(outI)))
              outI += 1
              columnI += 1
            }
            //now, map the NNZ value
            out.dc_update(outI, func(collA.dc_apply(idxA), collB.dc_apply(outI)))
            columnI += 1
            outI += 1
            idxA += 1
          }
          //now, finish off mapping any remaining zero values in this row
          while(columnI < collA.numCols){
            out.dc_update(outI, func(ops.zero, collB.dc_apply(outI)))
            outI += 1
            columnI += 1
          }

        } //end mapping this (non-empty) row

      row += 1
    }


    //synchronize here //TODO
    var ci = 0
    val cend = if (chunkDone != null) chunkDone.length else 0
    while (ci != cend) {
      while (!chunkDone(ci)) { }//busy waiting?
      ci += 1
    }
    out
  }

  private class SlaveChunk(collA: SparseMatrix[A], collB: DeliteCollection[B], out: C[R], pos: Int, total: Int)(implicit ops: ArithOps[A]) extends DeliteOP[DeliteUnit] {
    override def getImmutableDeps = null
    override def getMutableDeps = null

    def task = {
      val chunk_size = collA.numRows / total
      val remainder = collA.numRows % total
      val sz = if (pos < remainder) chunk_size + 1 else chunk_size
      val off = if (pos < remainder) 0 else pos - remainder

      var row = pos*(chunk_size+1) - off
      val end = row + sz
      var outI = row*collA.numCols
//      println("in slave chunk, row = " + row + " end = " + end)

      while (row != end) {
        if(collA.row_idx_apply(row) == -1){ //this row is empty

          var columnI = 0
          while(columnI < collA.numCols){ //TODO: is this really faster than a for loop?
            out.dc_update(outI, func(ops.zero, collB.dc_apply(outI)))
            outI += 1
            columnI += 1
          }
        }
        else{ //this row isn't empty
          //first, find the next non-empty row
          var nextRow = row + 1
          while(collA.row_idx_apply(nextRow) == -1) nextRow += 1
          //now, get that row's column index (so we know when to stop)
          val toStop = collA.row_idx_apply(nextRow)
          var idxA = collA.row_idx_apply(row)

          var columnI = 0 //iterate through this row's columns
          while(idxA < toStop){
            //iterate through zero values before the next NNZ value
            while(columnI != collA.col_idx_apply(idxA)){
              out.dc_update(outI, func(ops.zero, collB.dc_apply(outI)))
              outI += 1
              columnI += 1
            }
            //now, map the NNZ value
            out.dc_update(outI, func(collA.dc_apply(idxA), collB.dc_apply(outI)))
            columnI += 1
            outI += 1
            idxA += 1
          }
          //now, finish off any remaining zero values in this row
          while(columnI < collA.numCols){
            out.dc_update(outI, func(ops.zero, collB.dc_apply(outI)))
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

/*  //Old code, if other things dont work out 

    for(row <- 0 until collA.numRows){ //Iterate through each row of the SM

      if(collA.row_idx_apply(row) == -1){ //this row is empty

        var colI = 0
        while(colI < collA.numCols){ 
          out.dc_update(idxB, func(ops.zero, collB.dc_apply(idxB)))
          colI += 1
          idxB += 1
        }
      }
      else{ //this row isn't empty
        //first, find the next non-empty row
        var nextRow = row + 1
        while(collA.row_idx_apply(nextRow) == -1) nextRow += 1
        //now, get that row's column index (so we know when to stop)
        val toStop = collA.row_idx_apply(nextRow)

        var colI = 0 //iterate through this row's columns
        while(colI < collA.numCols){
          if(idxA >= toStop){ //if we've already used all this row's values
            out.dc_update(idxB, func(ops.zero, collB.dc_apply(idxB)))
            idxB += 1
          }
          else{ //if this row still has values
            if(colI == collA.col_idx_apply(idxA)){ //if we've reached a value
              out.dc_update(idxB, func(collA.dc_apply(idxA), collB.dc_apply(idxB)))
              idxB += 1
              idxA += 1
            }
            else{
              out.dc_update(idxB, func(ops.zero, collB.dc_apply(idxB)))
              idxB += 1
            }
          }

          colI += 1
        } //end iteration through this (non-empty) row

      }
    }*/
