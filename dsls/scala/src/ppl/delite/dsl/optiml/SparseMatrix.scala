package ppl.delite.dsl.optiml

import scala.reflect.ClassManifest
import ppl.delite.core.ops._
import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.core._
import ppl.delite.dsl.optiml.specialized._
import ppl.delite.cuda._
import collection.mutable.{ArrayBuffer, Cloneable}
import ppl.delite.dsl.optiml.SparseMatrix
import ppl.delite.dsl.primitive.{DeliteDouble, DeliteBoolean}

/**
 * Created by IntelliJ IDEA.
 * User: joe
 * Date: Jul 23, 2010
 * Time: 11:55:35 PM
 * To change this template use File | Settings | File Templates.
 */

object SparseMatrix{

  /********************Constructors and related stuff **********************/
  def apply[A : ClassManifest](xs: Matrix[A]) = newSparseMatrix(xs)
  def apply[A : ClassManifest](numRows: Int, numCols: Int) = newSparseMatrix[A](numRows,numCols)
  def apply[A : ClassManifest](rows: Int, cols: Int, rowI: Vector[Int], colI: Vector[Int], vals: Vector[A]) = newSparseMatrix[A](rows,cols,rowI,colI,vals)

  private def newSparseMatrix[A](xs: Matrix[A])(implicit m: ClassManifest[A]): SparseMatrix[A] = {
    m match {
      case ClassManifest.Double => DoubleSparseMatrixImpl.fromDoubleMatrix(xs.asInstanceOf[Matrix[Double]]).asInstanceOf[SparseMatrix[A]]
      //case ClassManifest.Float => new FloatMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Int => new IntMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Long => new LongMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Boolean => new BooleanMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case _ => new SparseMatrixImpl[A](numRows, numCols)
    }
  }

  private def newSparseMatrix[A](numRows: Int, numCols: Int)(implicit m: ClassManifest[A]): SparseMatrix[A] = {
    m match {
      case ClassManifest.Double => new DoubleSparseMatrixImpl(numRows, numCols).asInstanceOf[SparseMatrix[A]]
      //case ClassManifest.Float => new FloatMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Int => new IntMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Long => new LongMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Boolean => new BooleanMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case _ => new SparseMatrixImpl[A](numRows, numCols)
    }
  }

  private def newSparseMatrix[A](rows: Int, cols: Int, rowI: Vector[Int], colI: Vector[Int], vals: Vector[A])(implicit m: ClassManifest[A]): SparseMatrix[A] = {
    m match {
      case ClassManifest.Double => new DoubleSparseMatrixImpl(rows,cols,rowI,colI,vals.asInstanceOf[Vector[Double]]).asInstanceOf[SparseMatrix[A]]
      //case ClassManifest.Float => new FloatMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Int => new IntMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Long => new LongMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Boolean => new BooleanMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case _ => new SparseMatrixImpl[A](numRows, numCols)
    }
  }
  
  private def newSparseMatrix[A]()(implicit m: ClassManifest[A]): SparseMatrix[A] = {
      m match {
        case ClassManifest.Double => new DoubleSparseMatrixImpl().asInstanceOf[SparseMatrix[A]]
      }
    }


  def zeros(rows: Int, cols: Int) : SparseMatrix[Double] = {
    Delite.run(OP_zeros(rows, cols))
  }

  def fromVectors[A](rows: Int, cols: Int, rowI: Vector[Int], colI: Vector[Int], vals: Vector[A])(implicit man: ClassManifest[A]) : SparseMatrix[A] = {
    Delite.run(OP_fromVectors(rows,cols,rowI,colI,vals))
  }

  def fromMatrix[A](m: Matrix[A])(implicit man: ClassManifest[A]) : SparseMatrix[A] = {
    Delite.run(OP_fromMatrix(m))
     /*man match {
      case ClassManifest.Double => DoubleSparseMatrixImpl.fromDoubleMatrix(m.asInstanceOf[Matrix[Double]]).asInstanceOf[SparseMatrix[A]]
      //case ClassManifest.Float => new FloatMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Int => new IntMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Long => new LongMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case ClassManifest.Boolean => new BooleanMatrixImpl(numRows, numCols).asInstanceOf[Matrix[A]]
      //case _ => new SparseMatrixImpl[A](numRows, numCols)
    }*/
  }


  class ProxyFactory[T: ClassManifest] extends DeliteProxyFactory[SparseMatrix[T]] {
    def newProxy = newSparseMatrix[T]
  }

  protected[optiml] case class OP_zeros( numRows : Int, numCols : Int) extends DeliteOP_SingleTask[SparseMatrix[Double]]() {
    def task = {
      SparseMatrix[Double](numRows, numCols)
    }
  }

  protected[optiml] case class OP_fromMatrix[A](m: Matrix[A])(implicit man: ClassManifest[A]) extends DeliteOP_SingleTask[SparseMatrix[A]]() {
    def task = {
      SparseMatrix[A](m)
    }
  }

  protected[optiml] case class OP_fromVectors[A](rows: Int, cols: Int, rowI: Vector[Int], colI: Vector[Int], vals: Vector[A])
                  (implicit man: ClassManifest[A]) extends DeliteOP_SingleTask[SparseMatrix[A]]() {
    def task = {
      SparseMatrix[A](rows, cols,rowI,colI, vals)
    }
  }

  /********************** Arithmetic Ops ***********************/
  /*
  protected[optiml] case class OP_+[A](val collA: Matrix[A], val collB: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{
    override val associative = true
  }*/

  protected[optiml] case class OP_+[A](val collA: Matrix[A], val collB: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{
    override val associative = true

    val out = collA

    def func = (a,b) => ops.+(a,b)
  }

  protected[optiml] case class OP_-[A](val collA: Matrix[A], val collB: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{

    val out = collA
    def func = (a,b) => ops.-(a,b)
  }

  protected[optiml] case class OP_/[A](sm: SparseMatrix[A], m: Matrix[A]) (implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_MutableSingleTask[SparseMatrix[A]](m)(sm){

    def task = {

      var row = 0
      while(row < sm.numRows){

        if(sm.row_idx_apply(row) != -1){
          //find next row with values in it
          var nextRow = row+1
          while(sm.row_idx_apply(nextRow) == -1) nextRow += 1

          var rI = sm.row_idx_apply(row)
          while(rI < sm.row_idx_apply(nextRow)){
            sm.vals_update(rI,  ops./( sm.vals_apply(rI), m(row, sm.col_idx_apply(rI)) ) )
            rI += 1
          }
        }

        row += 1
      }

      sm
    }

  }
  protected[optiml] case class OP_mdot[A](sm: SparseMatrix[A], m: Matrix[A]) (implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_MutableSingleTask[SparseMatrix[A]](m)(sm){

    def task = {

      var row = 0
      while(row < sm.numRows){

        if(sm.row_idx_apply(row) != -1){
          //find next row with values in it
          var nextRow = row+1
          while(sm.row_idx_apply(nextRow) == -1) nextRow += 1

          var rI = sm.row_idx_apply(row)
          while(rI < sm.row_idx_apply(nextRow)){
            sm.vals_update(rI,  ops.*( sm.vals_apply(rI), m(row, sm.col_idx_apply(rI)) ) )
            rI += 1
          }
        }

        row += 1
      }

      sm
    }
  }

  protected[optiml] case class OP_plusEqualsNNZOnly[A](val sm: SparseMatrix[A], val m: Matrix[A])
    (implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_ForEach[Int, SparseMatrix[A]](m)(){

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
            sm.CSC_dc_update(cI, ops.+(sm.CSC_dc_apply(cI), m(sm.CSC_row_idx_apply(cI), col) ) )
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
            sm.vals_update(rI,  ops.+(sm.vals_apply(rI),  m(row, sm.col_idx_apply(rI)) ) )
            rI += 1
          }
        }

      }
    }
  }

  protected[optiml] case class OP_toMatrix[A](sm: SparseMatrix[A])(implicit pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_ForEach[Int, Matrix[A]](sm)(){

    val coll = Vector.range(0, sm.numRows)
    val out = Matrix[A](sm.numRows, sm.numCols)

    def func = row => {
      if(sm.row_idx_apply(row) != -1){
        //find next row with values in it
        var nextRow = row+1
        while(sm.row_idx_apply(nextRow) == -1) nextRow += 1

        var rI = sm.row_idx_apply(row)
        while(rI < sm.row_idx_apply(nextRow)){
          out(row, sm.col_idx_apply(rI)) = sm.vals_apply(rI)
          rI += 1
        }
      }
    }
  }

  //sparse matrix multiply
 /*
  protected[optiml] case class OP_smMult[A](m1: SparseMatrix[A], m2: SparseMatrix[A])(implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_ForEach[Int, Matrix[A]](m1, m2)(){

    m1.chkEquals(m1.numCols, m2.numRows)
    val coll = Vector.range(0, (m1.numRows * m2.numCols))
    val out = Matrix[A](m1.numRows, m2.numCols)

    def func = index => {
      val row = index / m2.numCols
      val col = index % m2.numCols


      val thisRowI = m1.row_idx_apply(row)
      if(thisRowI == -1)
        out(row, col) = ops.zero

      val thisColI = m2.CSC_col_idx_apply(col)
      if(thisColI == -1)
        out(row, col) = ops.zero

      if(thisRowI != -1 && thisColI != -1){
        var nextRow = row + 1  //find nextRow
        while(m1.row_idx_apply(nextRow) == -1) nextRow += 1

        var nextCol = col + 1  //find nextRow
        while(m2.CSC_col_idx_apply(nextCol) == -1) nextCol += 1

        var sum = ops.zero
        var rI = thisRowI      //take this out later
        var cI = thisColI//m2.CSC_col_idx_apply(col)
        val toStopRI = m1.row_idx_apply(nextRow)
        val toStopCI = m2.CSC_col_idx_apply(nextCol)
        while((cI < toStopCI) && (rI < toStopRI)){

          if(m1.col_idx_apply(rI) == m2.CSC_row_idx_apply(cI)){ //we've found two NNZ values to multiply
            sum = ops.+(sum, ops.*(m1.vals_apply(rI), m2.CSC_dc_apply(cI)))

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
    }
  }*/

  //sparse matrix multiply

  protected[optiml] case class OP_smMult[A](m1: SparseMatrix[A], m2: SparseMatrix[A])(implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_ForEach[Int, Matrix[A]](m1, m2)(){

    m1.chkEquals(m1.numCols, m2.numRows)
    val coll = Vector.range(0, (m1.numRows * m2.numCols))
    val out = Matrix[A](m1.numRows, m2.numCols)

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

            var sum = ops.zero
            var rI = thisRowI
            var cI = m2.CSC_col_idx_apply(col)
            val toStopRI = m1.row_idx_apply(nextRow)
            val toStopCI = m2.CSC_col_idx_apply(nextCol)
            while((cI < toStopCI) && (rI < toStopRI)){

              if(m1.col_idx_apply(rI) == m2.CSC_row_idx_apply(cI)){ //we've found two NNZ values to multiply
                sum = ops.+(sum, ops.*(m1.vals_apply(rI), m2.CSC_dc_apply(cI)))

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
  
  protected[optiml] case class OP_*[A](m1: SparseMatrix[A], m2: Matrix[A])(implicit ops: ArithOps[A], pFact: Matrix.ProxyFactory[A], c: ClassManifest[A])
    extends DeliteOP_ForEach[Int, Matrix[A]](m1, m2)(){

    //what is lifting?
    m1.chkEquals(m1.numCols, m2.numRows)

    val coll = Vector.range(0, (m1.numRows))
    val out = Matrix[A](m1.numRows, m2.numCols)

    def func = row => {
      val thisRowI = m1.row_idx_apply(row)
      if(thisRowI != -1){ //if this row of values should be non-zero
        //find next non-zero row
        var nextRow = row + 1
        while(m1.row_idx_apply(nextRow) == -1) nextRow += 1

        var col = 0
        while(col < m2.numCols){ //iterate through all cols of m2

          var sum = ops.zero
          var rI = thisRowI
          while(rI < m1.row_idx_apply(nextRow)){ //loop through this rows NNZ values
            sum = ops.+(sum, ops.*(m1.vals_apply(rI), m2(m1.col_idx_apply(rI) ,col)))
            rI += 1
          }

          out(row,col) = sum
          col += 1
        }
      }
    }
  }

  protected[optiml] case class OP_>[A](val collA: Matrix[A], val collB: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{

    def func = (a,b) => ops.>(a,b)
  }

  protected[optiml] case class OP_<[A](val collA: Matrix[A], val collB: Matrix[A], val out: Matrix[A])(implicit ops: ArithOps[A], c: ClassManifest[A])
    extends DeliteOP_ZipWith2[A,A,A,Matrix]{

    def func = (a,b) => ops.<(a,b)
  }


  protected[optiml] case class OP_vprod[A](m: SparseMatrix[A], v: Vector[A], val out: Vector[A])
    (implicit ops: ArithOps[A], pFact: Vector.ProxyFactory[A], c: ClassManifest[A]) extends DeliteOP_Map[Int, A, Vector]{

    val coll = Vector.range(0, m.numRows)

    def func = row => {
      var rowI = m.row_idx_apply(row)
      var out = ops.zero
      if(rowI != -1){ //if row isnt empty
        //find next non-empty row index
        var nextRow = row + 1
        while(m.row_idx_apply(nextRow) == -1) nextRow += 1

        //now, iterate over all the values in this row and multiply values
        while(rowI < m.row_idx_apply(nextRow)){
          out = ops.+(out, ops.*(m.vals_apply(rowI),  v(m.col_idx_apply(rowI))) )
          rowI += 1
        }
      }

      out
    }

  }

  /* This allocates a vector of size m.numrows to help with vector multiplication */
  protected[optiml] case class OP_allocVHelper[A: ClassManifest](m: SparseMatrix[A]) extends DeliteOP_SingleTask[Vector[Int]](m) {
    def task = {
      val out = Vector[Int](m.numRows)
      var i = 0
      while(i < m.numRows){
        out(i) = i
        i+= 1
      }
      out
    }
  }

  //another helper that allocates a vector the size of m.numRows
  protected[optiml] case class OP_allocV[A,B: ClassManifest](m: SparseMatrix[A]) extends DeliteOP_SingleTask[Vector[B]](m) {
    def task = {
      Vector[B](m.numRows)
    }
  }

  /* This allocates a dense matrix, sinse currently, all outputs are dense */
  protected[optiml] case class OP_alloc[A,B: ClassManifest](m: SparseMatrix[A]) extends DeliteOP_SingleTask[Matrix[B]](m) {
    def task = {
      Matrix[B](m.numRows, m.numCols)
    }
  }

  protected[optiml] case class OP_sparsity[A: ClassManifest](m: SparseMatrix[A]) extends DeliteOP_SingleTask[DeliteDouble](m){
    def task = {
      DeliteDouble((m.size/2.0)/(m.numRows*m.numCols))
    }
  }

  protected[optiml] case class OP_map[A, B : ClassManifest](val coll: SparseMatrix[A], val out: SparseMatrix[B], val func: A => B)
    extends DeliteOP_Map[A,B,SparseMatrix]
                                                                 
  protected[optiml] case class OP_SparseMap[A, B](val coll: SparseMatrix[A], val out: Matrix[B], val func: A => B)(implicit ops: ArithOps[A], man: ClassManifest[B])
     extends DeliteOP_SparseMap[A,B,Matrix]
}

trait SparseMatrix[@specialized(Double) T] extends DeliteCollection[T] with Cloneable[SparseMatrix[T]] {
  import SparseMatrix._

// public (read-only) properties
  def numRows : Int
  def numCols : Int
  def frozen: Boolean
  def numElems : Int
  def isBoth : Boolean //if CSR and CSC are enabled
  def size : Int

  //TODO: possibly improve this, this just chunks values in _vals
  def chunk(start: Int, end: Int) = new Iterator[T] {
    private var index = start
    private val ceil = if (end+1 <= SparseMatrix.this.size) end+1 else SparseMatrix.this.size+1
    def hasNext = (index < ceil)
    def next = {
      index += 1
      dc_apply(index-1)
    }
  }

  def col_idx_apply(i: Int) : Int
  def row_idx_apply(i: Int) : Int
  def apply(i: Int, j: Int) : T
  def update(i: Int, j: Int, x: T)
  def CSC_dc_apply(i: Int) : T
  def CSC_dc_update(i: Int, x: T)
  def vals_apply(i : Int) : T
  def vals_update(i: Int, x: T)
  def cleanup

  def toDoubleMatrix : Matrix[Double]
  def trans : SparseMatrix[T]
  def selfTrans

  //For col and row axess in CSC format
  def CSC_col_idx_apply(i: Int) : Int
  def CSC_row_idx_apply(i: Int) : Int

//  def clone : SparseMatrix[T]

  def pprint = {
    for (i <- 0 until numRows){
      print("[ ")
      for (j <- 0 until numCols){
        print(this(i,j))
        print(" ")
      }
      print("]\n")
    }
  }

  def debug
  def debugCSC

  /**************************** Arithmetic Operations ****************************/


  /* point-wise operations */
  def +(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_+(this.toMatrix,m))
  }

  def -(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_-(this.toMatrix,m))
  }
  
  def /(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : SparseMatrix[T] = {
    run(OP_/(this.clone, m))
  }

  def dot(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : SparseMatrix[T] = {
    run(OP_mdot(this.clone, m))
  }

  //special operation for RF_RBM that adds only the values of the input matrix which
  //correspond with the NNZ values of the sparse matrix to the sparse matrix
  def plusEqualsNNZOnly(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : SparseMatrix[T] = {
    run(OP_plusEqualsNNZOnly(this, m))
//    throw new Exception("should be overridden!")
  }

  /* scalar operations */

  def +(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = this.toMatrix
    run(Matrix.OP_map[T,T](out, out, e => ops.+(e,d)))
  }

  def -(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = this.toMatrix
    run(Matrix.OP_map[T,T](out, out, e => ops.-(e,d)))
  }

  def *(d : T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : SparseMatrix[T] = {
    run(OP_map[T,T](this, this.clone, e => ops.*(e,d)))
  }

  def *=(d : T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : SparseMatrix[T] = {
    run(OP_map[T,T](this, this, e => ops.*(e,d)))
  }

  def /(d : T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : SparseMatrix[T] = {
    run(OP_map[T,T](this, this.clone, e => ops./(e,d)))
  }

  def /=(d : T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : SparseMatrix[T] = {
    run(OP_map[T,T](this, this, e => ops./(e,d)))
  }

  /************* vector/matrix operations *****************/

  def *(v : Vector[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Vector[T] = {
    run(OP_vprod(this, v, run(OP_allocV[T,T](this))))
  }

  /* Sparse Matrix-Dense Matrix multiplication */
  def *(b: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_*(this, b))
  }

  def *(sm: SparseMatrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    run(OP_smMult(this, sm))
  }

  def >(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = this.toMatrix
    run(OP_>(out,m, out))
  }

  def >(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = this.toMatrix
    run(Matrix.OP_map[T,T](out, out, e => ops.>(e,d)))
  }

  def <(m: Matrix[T])(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = this.toMatrix
    run(OP_<(out,m, out))
  }

  def <(d: T)(implicit ops: ArithOps[T], c: ClassManifest[T]) : Matrix[T] = {
    var out = this.toMatrix
    run(Matrix.OP_map[T,T](out, out, e => ops.<(e,d)))
  }


  //misc. OPs

  def chkEquals(x: Int, y: Int) {
    if (x != y) throw new IllegalArgumentException
  }

  def toMatrix()(implicit c: ClassManifest[T]) : Matrix[T] = {
    run(OP_toMatrix(this))
  }

  def sparsity()(implicit c: ClassManifest[T]) : Double = {
    run(OP_sparsity(this))
  }

  def mapNNZ(f: T => T)(implicit c: ClassManifest[T]) : SparseMatrix[T] =  {
    run(OP_map[T,T](this, this.clone, f))
  }



}