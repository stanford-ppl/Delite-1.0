package ppl.delite.dsl.optiml.specialized

import ppl.delite.core.appinclude._
import ppl.delite.core.include._
import ppl.delite.nativeGPU.GPUable
import ppl.delite.dsl.optiml.{ArithOps, Vector, VectorView, Matrix, SparseMatrix}
import collection.mutable.ArrayBuffer
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.core.ops.{DeliteOP_ForEach, DeliteOP_MutableSingleTask, DeliteOP_SingleTask}
import ppl.delite.core.{Delite, DeliteUnit}

/**
 * Created by IntelliJ IDEA.
 * User: joe
 * Date: Jul 24, 2010
 * Time: 12:22:14 AM
 * To change this template use File | Settings | File Templates.
 */

object DoubleSparseMatrixImpl{

  def fromDoubleMatrix(m: Matrix[Double]) : SparseMatrix[Double] = {
    Delite.run(OP_fromDoubleMatrix(m))
  }

  protected[optiml] case class OP_clone(sm: DoubleSparseMatrixImpl) extends DeliteOP_SingleTask[SparseMatrix[Double]](sm) {
     def task = {
       var toreturn = new DoubleSparseMatrixImpl(sm.numRows, sm.numCols)
       toreturn._rowInd = sm._rowInd.clone
       toreturn._colInd = sm._colInd.clone
       toreturn._vals = sm._vals.clone
       toreturn._nElems = toreturn._vals.length
       if(sm._isBoth){
         toreturn._colIndCSC = sm._colIndCSC.clone
         toreturn._rowIndCSC = sm._rowIndCSC.clone
         toreturn._valsCSC = sm._valsCSC.clone
         toreturn._isBoth = true
       }
       else{
         toreturn._isBoth = false
       }

       toreturn
     }
  }

  protected[optiml] case class OP_trans(sm: DoubleSparseMatrixImpl) extends DeliteOP_SingleTask[SparseMatrix[Double]](sm) {
    def task = {
      var tr = new DoubleSparseMatrixImpl(sm.numCols, sm.numRows)
      tr._rowInd = sm._colIndCSC.clone
      tr._colInd = sm._rowIndCSC.clone
      tr._vals = sm._valsCSC.clone
      tr._colIndCSC = sm._rowInd.clone
      tr._rowIndCSC = sm._colInd.clone
      tr._valsCSC = sm._vals.clone
      tr._nElems = sm._nElems
      tr
    }
  }

  protected[optiml] case class OP_selfTrans(sm: DoubleSparseMatrixImpl) extends DeliteOP_MutableSingleTask[DeliteUnit]()(sm){
    def task = {
      //backup things
      val bRowInd = sm._rowInd
      val bColInd = sm._colInd
      val bVals = sm._vals
      val bNCols = sm._nCols

      //replace things
      sm._rowInd = sm._colIndCSC
      sm._colInd = sm._rowIndCSC
      sm._vals = sm._valsCSC
      sm._rowIndCSC = bColInd
      sm._colIndCSC = bRowInd
      sm._valsCSC = bVals

      sm._nCols = sm._nRows
      sm._nRows = bNCols

    }
  }

  protected[optiml] case class OP_cleanup(sm: DoubleSparseMatrixImpl) extends DeliteOP_MutableSingleTask[DeliteUnit]()(sm){
    def task = {
      //first, iterate through rows and cleanup
      var row = sm._rowInd.length - 2 //the last row (sm.numRows - 1)
      var nextRow = row + 1
      while(row > -1){

        var rowI = sm._rowInd(row)
        if(rowI != -1){ //iterate through all nnz rows

          var nextRowI = sm._rowInd(nextRow)
          var zerosRemoved = 0

          //iterate through all values from rowI to nextRowI
          while(rowI < nextRowI){
            //if this elem is 0, then remove it
            if(sm._vals(rowI) == 0.0){
              sm._vals.remove(rowI)
              sm._colInd.remove(rowI)
              zerosRemoved += 1
              nextRowI -= 1
            }
            else{ //otherwise, move to the next space
              rowI += 1
            }
          }

          //dont forget to adjust numElems!
          sm._nElems -= zerosRemoved

          //now, adjust row indeces accordingly
          var rTC = nextRow
          while(rTC < sm._rowInd.length){
            if(sm._rowInd(rTC) != -1){
              sm._rowInd(rTC) = sm._rowInd(rTC) - zerosRemoved
            }
            rTC += 1
          }

          //now, set new bounds
          nextRow = row
        }

        row -= 1
      }

      //iterate through CSC columns and cleanup
      var col = sm._colIndCSC.length - 2 // the last col
      var nextCol = col + 1
      while(col > -1){

        var colI = sm._colIndCSC(col)
        if(colI != -1){

          var nextColI = sm._colIndCSC(nextCol)
          var zerosRemoved = 0

          //iterate through all values from colI to nextColI
          while(colI < nextColI){
            //remove this element if its zero
            if(sm._valsCSC(colI) == 0.0){
              sm._valsCSC.remove(colI)
              sm._rowIndCSC.remove(colI)
              zerosRemoved += 1
              nextColI -= 1
            }
            else{ //otherwise, move to next space
              colI += 1
            }
          }

          //now, adjust col indeces accordingly
          var cTC = nextCol
          while(cTC < sm._colIndCSC.length){
            if(sm._colIndCSC(cTC) != -1){
              sm._colIndCSC(cTC) = sm._colIndCSC(cTC) - zerosRemoved
            }
            cTC += 1
          }

          //set nextCol for next iteration through a non-zero row
          nextCol = col
        }

        col -= 1
      }


      //now, finally go through and fix any rows/CSC columns that have entries in zero rows/CSC cols
      nextRow = sm._rowInd.length - 1
      row = nextRow - 1
      while(row > -1){

        val rowI = sm._rowInd(row)
        if(rowI != -1){

          if(rowI == sm._rowInd(nextRow)){ //if row actually is a zero-row
            sm._rowInd(row) = -1
          }
          else{
            nextRow = row
          }
        }

        row -= 1
      }

      nextCol = sm._colIndCSC.length -1
      col = nextCol -1
      while(col > -1){

        val colI = sm._colIndCSC(col)
        if(colI != -1){

          if(colI == sm._colIndCSC(nextCol)){
            sm._colIndCSC(col) = -1
          }
          else{
            nextCol = col
          }
        }

        col -= 1
      }

    }
  }

  protected[optiml] case class OP_toDoubleMatrix(sm: DoubleSparseMatrixImpl) extends DeliteOP_SingleTask[Matrix[Double]](sm) {
    def task = {
      var toReturn = Matrix.zeros(sm._nRows, sm._nCols)

      for(i <- 0 until (sm._rowInd.length -1)){ //iterate through all rows in this SM
        if(sm._rowInd(i) != -1){                //with values
          var ind = sm._rowInd(i)

          var nextRow = i + 1 //find next row with non-zero entries
          while(sm._rowInd(nextRow) == -1)
            nextRow = nextRow + 1

          //now, iterate from ind to _rowInd(nextRow) to get all of this rows entries
          while(ind < sm._rowInd(nextRow)){
            toReturn(i, sm._colInd(ind)) = sm._vals(ind)
            ind += 1
          }

        }
      }

      toReturn
    }
  }

  protected[optiml] case class OP_apply(sm : DoubleSparseMatrixImpl, i: Int, j: Int) extends DeliteOP_SingleTask[DeliteDouble](sm) {
    def task : DeliteDouble = {
      if(sm._rowInd(i) == -1) return 0.0
      else{
        var startInd = sm._rowInd(i)
        var nextRow = i + 1 //find next row with non-zero entries
        while(sm._rowInd(nextRow) == -1)
          nextRow = nextRow + 1

        //now, iterate through colInd and see if the matrix contains this entry
        while(startInd < sm._rowInd(nextRow)){
          if(sm._colInd(startInd) == j) return sm._vals(startInd)
          startInd = startInd + 1
        }
      }
      return 0.0
    }
  }

  protected[optiml] case class OP_fromDoubleMatrix(m : Matrix[Double]) extends DeliteOP_ForEach[Int, SparseMatrix[Double]](m)(){

    val out = new DoubleSparseMatrixImpl(m.numRows, m.numCols)
    val coll = Vector.range(0,2)

    def func = tsk => {
      if(tsk == 0){
        for(i <- 0 until m.numRows){
          var valueInThisRow = false

          for(j <- 0 until m.numCols){
            //iterate through all non-zero elements, and put them in order

            if(m(i,j) != 0.0){
              if(valueInThisRow == false){ //if this is the first non-zero value in the row
                out._rowInd(i) = out._nElems
                valueInThisRow = true
              }
              //Now, put this element in the SparseMatrix and update counts
              out._colInd.append(j)
              out._vals.append(m(i,j))
              out._nElems += 1

            }

          }
        }
        //finally, update the last pointer in rowind to point to the end of colInd
        out._rowInd(out._rowInd.length - 1) = out._colInd.length
      }


      else if(tsk == 1){
        /************** CSC **************************/
        var temp = 0
        for(j <- 0 until m.numCols){
          var valueInThisCol = false

          for(i <- 0 until m.numRows){
            //iterate through all non-zero elements, and put them in order

            if(m(i,j) != 0.0){
              if(valueInThisCol == false){ //if this is the first non-zero value in the row
                out._colIndCSC(j) = temp
                valueInThisCol = true
              }
              //Now, put this element in the SparseMatrix and update counts
              out._rowIndCSC.append(i)
              out._valsCSC.append(m(i,j))
              temp += 1
            }
          }
        }
        //finally, update the last pointer in rowind to point to the end of colInd
        out._colIndCSC(out._colIndCSC.length - 1) = out._rowIndCSC.length
      }
    }
  }

  protected[optiml] case class OP_update(sm : DoubleSparseMatrixImpl, i:Int, j:Int, x:Double) extends DeliteOP_MutableSingleTask[DeliteUnit]()(sm){
    def task = {
      //first, figure out if this value is in the matrix
      var alreadyInMatrix = (sm._rowInd(i) != -1)
      if(alreadyInMatrix){
        var startInd = sm._rowInd(i)
        var nextRow = i + 1 //find next row with non-zero entries
        while(sm._rowInd(nextRow) == -1)
          nextRow += 1

        val toStopAt = sm._rowInd(nextRow)
        //now, iterate through colInd and see if the matrix contains this entry
        while(startInd < toStopAt){
          if(sm._colInd(startInd) == j){ //the matrix does contain this entry
            startInd = toStopAt + 1 // break while loop
          }
          else if(sm._colInd(startInd) > j){ //the matrix doesn't contain this entry
            alreadyInMatrix = false
            startInd = toStopAt + 1 // break while loop
          }
          startInd = startInd + 1

          if(startInd == toStopAt) //if loop wasnt broken out of, the value isn't in sm
            alreadyInMatrix = false
        }
      }

      if(alreadyInMatrix) { //if this value is already in the matrix, just change it
        var beginIndex = sm._rowInd(i)
        while(sm._colInd(beginIndex) != j)
          beginIndex = beginIndex +1

        sm._vals(beginIndex) = x

        if(sm._isBoth){ //now, update the CSC representation
          var beginIndex = sm._colIndCSC(j)
          while(sm._rowIndCSC(beginIndex) != i)
            beginIndex += 1

          sm._valsCSC(beginIndex) = x
        }
      }
      else{ //otherwise, add the new member
        //first, find the next row above i with entries in it
        var nextRow = i+1
        while(sm._rowInd(nextRow) == -1) nextRow += 1

        var indexToInsert = -1

        //now, deal with incrementing _rowInd values
        if(sm._rowInd(i) == -1) { //if theres no values in this row already
          sm._rowInd(i) = sm._rowInd(nextRow)
          indexToInsert = sm._rowInd(i) //because there are no values at this row already, we already know to insert here
        }

        //now, increment all non -1 values by 1 from nextRow on
        var nextInd = nextRow
        while(nextInd < sm._rowInd.length){
          if(sm._rowInd(nextInd) != -1) sm._rowInd(nextInd) += 1
          nextInd += 1
        }

        //finished modifying rowInd
        //now find the right place to insert
        if(indexToInsert == -1){  //if we havent found indextoInsert yet
          val endThisRow = sm._rowInd(nextRow) - 1 //the first value that doesn't belong to this row
          var cInd = sm._rowInd(i)

          while(cInd < endThisRow){
            if(sm._colInd(cInd) > j){ //we've found the place to insert (assuming all col indeces are sorted)
              indexToInsert = cInd
              cInd = endThisRow //break while loop
            }
            cInd += 1
          }
            //if nothing has been found, this col value is the biggest in the row,
            // so put it in at the end of this col's values
          if(indexToInsert == -1) indexToInsert = endThisRow
        }

        //now, finally, insert j and x at indexToInsert
        sm._colInd.insert(indexToInsert, j)
        sm._vals.insert(indexToInsert, x)
        sm._nElems += 1

        // ******************************* begin CSC update ***********************************************
        if(sm._isBoth){ //now, do it all over and update CSC stuff
          //find next col with entries
          var nextCol = j+1
          while(sm._colIndCSC(nextCol) == -1) nextCol += 1

          indexToInsert = -1

          if(sm._colIndCSC(j) == -1){ //if theres no values in this col already
            sm._colIndCSC(j) = sm._colIndCSC(nextCol)
            indexToInsert = sm._colIndCSC(j) //insert at this new space
          }
          //now, increment all non -1 values by 1 from nextCol on
          nextInd = nextCol
          while(nextInd < sm._colIndCSC.length){
            if(sm._colIndCSC(nextInd) != -1) sm._colIndCSC(nextInd) += 1
            nextInd += 1
          }

          //finished modifying colInd, now find right place to insert (if we havent already)
          if(indexToInsert == -1){
            val endThisCol = sm._colIndCSC(nextCol) - 1
            var rInd = sm._colIndCSC(j)

            while(rInd < endThisCol){
              if(sm._rowIndCSC(rInd) > i){ //if we've found place to insert
                indexToInsert = rInd
                rInd = endThisCol //break while loop
              }
              rInd += 1
            }

            //if nothing has been found, this row val is biggest in the col, so put it at end
            if(indexToInsert == -1) indexToInsert = endThisCol
          }

          //now insert i and x at indexToInsert
          sm._rowIndCSC.insert(indexToInsert, i)
          sm._valsCSC.insert(indexToInsert, x)
        }
      }
    }
  }
}

private[optiml] class DoubleSparseMatrixImpl extends DoubleSparseMatrix /* with GPUable[Double] */ {
  import DoubleSparseMatrixImpl._

  type DSLType = DoubleSparseMatrixImpl

  //This implementation (currently) uses the CSR format for storing sparse rows

  protected[optiml] var _nRows = -1
  protected[optiml] var _nCols = -1
  protected[optiml] var _nElems = -1
  protected[optiml] var _frozen = false
  protected[optiml] var _vals: ArrayBuffer[Double] = null
  protected[optiml] var _colInd: ArrayBuffer[Int] = null
  protected[optiml] var _rowInd: Array[Int] = null
  /*CSC format*/
  protected[optiml] var _valsCSC: ArrayBuffer[Double] = null  //TODO update these guys
  protected[optiml] var _colIndCSC: Array[Int] = null
  protected[optiml] var _rowIndCSC: ArrayBuffer[Int] = null
  protected[optiml] var _isBoth: Boolean = false 

  def size : Int = {
    if(_isBoth)
      return force._vals.length * 2
    else
      return force._vals.length
  }

  override def concretize = {
    _vals = cvalue._vals
    _nRows = cvalue._nRows
    _nCols = cvalue._nCols
    _nElems = cvalue._nElems
    _colInd = cvalue._colInd
    _rowInd = cvalue._rowInd
    /*CSC*/
    _valsCSC = cvalue._valsCSC
    _colIndCSC = cvalue._colIndCSC
    _rowIndCSC = cvalue._rowIndCSC
    _isBoth = cvalue._isBoth
    cvalue = this
  }

  def init() = {
    cvalue = this
    isComputed = true
  }

  def this(nRows: Int, nCols: Int) = {
    this()
    _nRows = nRows
    _nCols = nCols
    _nElems = 0
    _colInd = new ArrayBuffer[Int]() //TODO: figure out a good size to initialize to
    _vals = new ArrayBuffer[Double]()
    _rowInd = new Array[Int](nRows + 1)

    /*CSC*/   // - this constructor initializes CSC by default
    _colIndCSC = new Array[Int](nCols + 1)
    _rowIndCSC = new ArrayBuffer[Int]()
    _valsCSC = new ArrayBuffer[Double]()

    //initialize everything in rowInd to -1 (cause there are no non-zero values in matrix yet)
    for(i <- 0 until (_rowInd.length - 1)) _rowInd(i) = -1
    for(i <- 0 until (_colIndCSC.length - 1)) _colIndCSC(i) = -1
    //initialize the bottom entry in _rowInd to contain the length of _colInd
    _rowInd(_rowInd.length - 1) = _colInd.length
    _colIndCSC(_colIndCSC.length - 1) = _rowIndCSC.length

    _isBoth = true

    init
  }

  //TODO: update this to not use the update function if we switch back to arrays or want this to be faster
  def this(nRows: Int, nCols: Int, rows: Vector[Int], cols: Vector[Int], vals: Vector[Double]) = {
    this(nRows, nCols)
    //_nElems = vals.length
    for(i <- 0 until vals.length){
      this.update(rows(i), cols(i), vals(i))
    }
    init
  }


 /* def this(m: Matrix[Double]) = {

    this()
    run(OP_fromDoubleMatrix(m))
    init
  }*/

  //TODO: remove this when we dont need to debug
  def debug() = {
    for(i <- 0 until _colInd.length){
      println(i+ " _colInd: "+ _colInd(i)+"  val: "+_vals(i))
    }
    for(i <- 0 until _rowInd.length){
      println("row "+i+" rowind " +_rowInd(i))
    }
    println("numelems " + _nElems)

  }

  def debugCSC() = {
    println("CSC Debug")
    if(_rowIndCSC != null && _valsCSC != null)
      println("its properly initialized!")
    for(i <- 0 until _rowIndCSC.length){
      println(i+ " _rowInd: "+ _rowIndCSC(i)+"  val: "+_valsCSC(i))
    }
    for(i <- 0 until _colIndCSC.length){
      println("column "+i+" colInd " +_colIndCSC(i))
    }
  }

  def numRows = force._nRows
  def numCols = force._nCols
  def frozen = force._frozen
  def numElems = force._nElems

  override def isBoth = force._isBoth

  def dc_update(n: Int, x: Double) = {
    if(n < _vals.length)
      _vals(n) = x
    else
      _valsCSC(n - _vals.length) = x
  }

  def dc_apply(n: Int) : Double = {
    if(n < _vals.length)
      return _vals(n)
    else
      return _valsCSC(n - _vals.length)
  }

  def vals_update(n: Int, x: Double) = {
    _vals(n) = x
  }

  def vals_apply(n: Int) : Double = {
    _vals(n)
  }

  def CSC_dc_apply(n: Int) : Double = {
    _valsCSC(n)
  }

  def CSC_dc_update(n: Int, x: Double) = {
    _valsCSC(n) = x
  }

  def row_idx_apply (i: Int) : Int = {
    _rowInd(i)
  }

  def col_idx_apply (i: Int) : Int = {
    _colInd(i)
  }

  def CSC_row_idx_apply (i: Int) : Int = {
    _rowIndCSC(i)
  }

  def CSC_col_idx_apply (i: Int) : Int = {
    _colIndCSC(i)
  }

  def apply (i: Int, j: Int) : Double = {
    run(OP_apply(this,i,j))
  }

  //TODO: possibly assert that (i,j) is in bounds?
  def update (i: Int, j: Int, x: Double) = {
    run(OP_update(this,i,j,x))
  }

  def toDoubleMatrix() : Matrix[Double] = {
    run(OP_toDoubleMatrix(this))
  }

  override def clone : SparseMatrix[Double] = {
    run(OP_clone(this))(doubleSMatFactory)
  }

  override def trans : SparseMatrix[Double] = {
    run(OP_trans(this))(doubleSMatFactory)
  }

  override def selfTrans {
    run(OP_selfTrans(this))
  }

  def cleanup = {
    run(OP_cleanup(this))
  }
}