package ppl.delite.dsl.optiml.specialized

import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.cuda._
import ppl.delite.metrics._
import ppl.delite.core._
import ppl.delite.core.appinclude._
import ppl.delite.core.ops._
import ppl.delite.core.ops.specialized._
import ppl.delite.cnative._
import ppl.delite.dsl.optiml.{ArithOps, Matrix, Vector, SparseMatrix}
import collection.mutable.ArrayBuffer
import ppl.delite.dsl.optiml.SparseMatrix._


/**
 * Created by IntelliJ IDEA.
 * User: joe
 * Date: Jul 24, 2010
 * Time: 12:02:25 AM
 * To change this template use File | Settings | File Templates.
 */

object DoubleSparseMatrix{

  protected[optiml] case class OP_+(val collA: Matrix[Double], val collB: Matrix[Double])
    extends DeliteOP_ZipWith2[Double,Double,Double,Matrix]{
    override val associative = true

    val out = collA

    def func = (a,b) => {a + b}
  }

  protected[optiml] case class OP_-(val collA: Matrix[Double], val collB: Matrix[Double])
    extends DeliteOP_ZipWith2[Double,Double,Double,Matrix]{

    val out = collA
    def func = (a,b) => {a-b}
  }

  protected[optiml] case class OP_plusEqualsNNZOnly(val sm: SparseMatrix[Double], val m: Matrix[Double])
    extends DeliteOP_ForEach[Int, SparseMatrix[Double]](m)(){

    val coll = Vector.range(0, sm.numRows + sm.numCols)
    val out = sm

    def func = row => {
      if(row >= sm.numRows) { //if this should be a column update
        val col = row - sm.numRows

        if(sm.CSC_col_idx_apply(col) != -1){ // if this row is non-empty

          //find next row with values in it
          var nextCol = col+1
          var nextColI = sm.CSC_col_idx_apply(nextCol)
          while(nextColI == -1){
            nextCol += 1
            nextColI = sm.CSC_col_idx_apply(nextCol)
          }

          //increment nnz values
          var cI = sm.CSC_col_idx_apply(col)
          while(cI < nextColI){
            sm.CSC_dc_update(cI, sm.CSC_dc_apply(cI) + m(sm.CSC_row_idx_apply(cI), col))
            cI += 1
          }

        }

      }
      else { //if this should be a row update

        if(sm.row_idx_apply(row) != -1){
          //find next row with values in it
          var nextRow = row+1
          while(sm.row_idx_apply(nextRow) == -1) nextRow += 1

          var rI = sm.row_idx_apply(row)
          while(rI < sm.row_idx_apply(nextRow)){
            sm.vals_update(rI,  sm.vals_apply(rI) + m(row, sm.col_idx_apply(rI))  )
            rI += 1
          }
        }

      }
    }
  }

  protected[optiml] case class OP_plusEqualsNNZOnly_Single(sm: SparseMatrix[Double], m: Matrix[Double])
    extends DeliteOP_MutableSingleTask[SparseMatrix[Double]](m)(sm){

    def task = {
      var row = 0
      while(row < sm.numRows){

        if(sm.row_idx_apply(row) != -1){
          //find next row with values in it
          var nextRow = row+1
          while(sm.row_idx_apply(nextRow) == -1) nextRow += 1

          var rI = sm.row_idx_apply(row)
          while(rI < sm.row_idx_apply(nextRow)){
            sm.vals_update(rI, sm.vals_apply(rI) + m(row, sm.col_idx_apply(rI)) )
            rI += 1
          }
        }

        row += 1
      }

      var col = sm.numCols -1
      while(col > -1){

        if(sm.CSC_col_idx_apply(col) != -1){ // if this row is non-empty

          //find next row with values in it
          var nextCol = col+1
          var nextColI = sm.CSC_col_idx_apply(nextCol)
          while(nextColI == -1){
            nextCol += 1
            nextColI = sm.CSC_col_idx_apply(nextCol)
          }

          //increment nnz values
          var cI = sm.CSC_col_idx_apply(col)
          while(cI < nextColI){
            sm.CSC_dc_update(cI, sm.CSC_dc_apply(cI) + m(sm.CSC_row_idx_apply(cI), col))
            cI += 1
          }

        }

        col -= 1
      }

      sm
    }
  }

  protected[optiml] case class OP_dot(sm: SparseMatrix[Double], /*smout: SparseMatrix[Double],*/ m: Matrix[Double])
    extends DeliteOP_MutableSingleTask[SparseMatrix[Double]]( m)(sm){

    def task = {
      var row = 0
      while(row < sm.numRows){

        if(sm.row_idx_apply(row) != -1){
          //find next row with values in it
          var nextRow = row+1
          while(sm.row_idx_apply(nextRow) == -1) nextRow += 1

          var rI = sm.row_idx_apply(row)
          while(rI < sm.row_idx_apply(nextRow)){
            sm.vals_update(rI, sm.vals_apply(rI) * m(row, sm.col_idx_apply(rI)) )
            rI += 1
          }
        }

        row += 1
      }

      sm
    }
  }

  protected[optiml] case class OP_/(sm: SparseMatrix[Double], m: Matrix[Double])
    extends DeliteOP_MutableSingleTask[SparseMatrix[Double]]( m)(sm){

    def task = {
      var row = 0
      while(row < sm.numRows){

        if(sm.row_idx_apply(row) != -1){
          //find next row with values in it
          var nextRow = row+1
          while(sm.row_idx_apply(nextRow) == -1) nextRow += 1

          var rI = sm.row_idx_apply(row)
          while(rI < sm.row_idx_apply(nextRow)){
            sm.vals_update(rI, sm.vals_apply(rI) / m(row, sm.col_idx_apply(rI)) )
            rI += 1
          }
        }

        row += 1
      }

      sm
    }
  }

  protected[optiml] case class OP_vprod(m: SparseMatrix[Double], v: Vector[Double], val out: Vector[Double])
     extends DeliteOP_Map[Int, Double, Vector]{

    val coll = Vector.range(0, m.numRows)

    def func = row => {
      var rowI = m.row_idx_apply(row)
      var out = 0.0
      if(rowI != -1){ //if row isnt empty
        //find next non-empty row index
        var nextRow = row + 1
        while(m.row_idx_apply(nextRow) == -1) nextRow += 1

        //now, iterate over all the values in this row and multiply values
        while(rowI < m.row_idx_apply(nextRow)){
          out += m.vals_apply(rowI) * v(m.col_idx_apply(rowI))
          rowI += 1
        }
      }
      out
    }

  }

  //sparse matrix multiply
  protected[optiml] case class OP_smMult(m1: SparseMatrix[Double], m2: SparseMatrix[Double])
    extends DeliteOP_ForEach[Int, Matrix[Double]](m1, m2)(){

    m1.chkEquals(m1.numCols, m2.numRows)
    val coll = Vector.range(0, (m1.numRows))
    val out = Matrix[Double](m1.numRows, m2.numCols)

    def func = row => {
      val thisRowI = m1.row_idx_apply(row)
      if(thisRowI != -1){ //if this row has values to multiply

        var nextRow = row + 1  //find nextRow
        while(m1.row_idx_apply(nextRow) == -1) nextRow += 1

        var col = 0
        while(col < m2.numCols){

          if(m2.CSC_col_idx_apply(col) != -1){ //if this entry should be non-zero
            //find bounds for next column
            var nextCol = col + 1
            while(m2.CSC_col_idx_apply(nextCol) == -1) nextCol += 1

            var sum = 0.0
            var rI = thisRowI
            var cI = m2.CSC_col_idx_apply(col)
            val toStopRI = m1.row_idx_apply(nextRow)
            val toStopCI = m2.CSC_col_idx_apply(nextCol)
            while((cI < toStopCI) && (rI < toStopRI)){

              if(m1.col_idx_apply(rI) == m2.CSC_row_idx_apply(cI)){ //we've found two NNZ values to multiply
                sum += m1.vals_apply(rI) * m2.CSC_dc_apply(cI)

                if((toStopRI - rI) < (toStopCI - cI)) cI += 1 //increment whichever has more values left
                else rI += 1

              }
              else if(m1.col_idx_apply(rI) < m2.CSC_row_idx_apply(cI)){
                rI += 1 //increment rI, which will increment m1.col_idx_apply(rI)
              }
              else{
                cI += 1
              }
            }
            out(row, col) = sum
          }

          col += 1
        }
      }
    }
  }

  protected[optiml] case class OP_*(m1: SparseMatrix[Double], m2: Matrix[Double])
    extends DeliteOP_ForEach[Int, Matrix[Double]](m1, m2)(){

    //what is lifting?
    m1.chkEquals(m1.numCols, m2.numRows)

    val coll = Vector.range(0, (m1.numRows))
    val out = Matrix[Double](m1.numRows, m2.numCols)

    def func = row => {
      val thisRowI = m1.row_idx_apply(row)
      if(thisRowI != -1){ //if this row of values should be non-zero
        //find next non-zero row
        var nextRow = row + 1
        while(m1.row_idx_apply(nextRow) == -1) nextRow += 1

        var col = 0
        while(col < m2.numCols){ //iterate through all cols of m2

          var sum = 0.0
          var rI = thisRowI
          while(rI < m1.row_idx_apply(nextRow)){ //loop through this rows NNZ values
            sum += m1.vals_apply(rI) * m2(m1.col_idx_apply(rI) ,col)
            rI += 1
          }

          out(row,col) = sum
          col += 1
        }
      }
    }
  }

  protected[optiml] case class OP_map(val coll: SparseMatrix[Double], val out: SparseMatrix[Double], val func: Double => Double)
    extends DeliteOP_Map[Double,Double,SparseMatrix]

}

trait DoubleSparseMatrix extends SparseMatrix[Double] {
  import DoubleSparseMatrix._

  protected[optiml] var _vals: ArrayBuffer[Double]
  protected[optiml] var _colInd: ArrayBuffer[Int]
  protected[optiml] var _rowInd: Array[Int]

  protected[optiml] var _valsCSC: ArrayBuffer[Double]
  protected[optiml] var _colIndCSC: Array[Int]
  protected[optiml] var _rowIndCSC: ArrayBuffer[Int]

  protected[optiml] var _isBoth: Boolean  

  override def dc_update(n: Int, x: Double)
  override def dc_apply(n: Int) : Double

  /* point-wise operations */
  override def +(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_+(this.toMatrix,m))
  }

  override def -(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_-(this.toMatrix,m))
  }

  override def plusEqualsNNZOnly(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : SparseMatrix[Double] = {
    run(OP_plusEqualsNNZOnly(this, m))
//    run(OP_plusEqualsNNZOnly_Single(this, m))
  }

  override def /(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : SparseMatrix[Double] = {
    run(OP_/(this.clone, m))
  }

  override def dot(m: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : SparseMatrix[Double] = {
    run(OP_dot(this.clone, m))
  }

  /* scalar operations */

   override def +(d: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
     var out = this.toMatrix
     run(Matrix.OP_map[Double,Double](out, out, e => e + d))
   }

   override def -(d: Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
     var out = this.toMatrix
     run(Matrix.OP_map[Double,Double](out, out, e => e - d))
   }

   override def *(d : Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : SparseMatrix[Double] = {
     run(OP_map(this, this.clone, e => e*d))
   }

   override def *=(d : Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : SparseMatrix[Double] = {
     run(OP_map(this, this, e => e*d))
   }

   override def /(d : Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : SparseMatrix[Double] = {
     run(OP_map(this, this.clone, e => e/d))
   }

   override def /=(d : Double)(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : SparseMatrix[Double] = {
     run(OP_map(this, this, e => e/d))
   }

    //vector-matrix operations...........
   override def *(v : Vector[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Vector[Double] = {
     run(OP_vprod(this, v, Vector[Double](this.numRows)))
   }

  override def *(b: Matrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_*(this, b))
  }

  override def *(sm: SparseMatrix[Double])(implicit ops: ArithOps[Double], c: ClassManifest[Double]) : Matrix[Double] = {
    run(OP_smMult(this, sm))
  }

  //other misc. ops

  override def mapNNZ(f: Double => Double)(implicit c: ClassManifest[Double]) : SparseMatrix[Double] =  {
    run(OP_map(this, this.clone, f))
  }

}