package ppl.delite.dsl.optiml.io

import ppl.delite.dsl.optiml.{Vector, Matrix}
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.core.appinclude._
import java.io.{BufferedReader, FileReader}
import scala.collection.mutable.ArrayBuffer

// file format is m lines with n floats per line, each float separated by whitespaces
// (same as matlab .dat)
object MLInputReader_Float{

  def read(filename: String) : Matrix[Float] = {
    val xfs = new BufferedReader(new FileReader(filename))
    var line = xfs.readLine()
    line = line.trim()
    var dbls = line.split("\\s+")
    val x = Matrix[Float](0, dbls.length)

    while (line != null){
      val v = Vector[Float](dbls.length)
      for (i <- 0 until dbls.length){
        v(i) = java.lang.Float.parseFloat(dbls(i))
      }
      x += v

      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        dbls = line.split("\\s+")
      }
    }
    xfs.close()

    x
  }

   def readVector(filename: String) : Vector[Float] = {
    val x = Vector[Float](0)

    val xfs = new BufferedReader(new FileReader(filename))
    var line = xfs.readLine()
    while (line != null){
      line = line.trim()
      val dbl = java.lang.Float.parseFloat(line)
      x += dbl

      line = xfs.readLine()
    }
    xfs.close()

    x
  }


  def getTokenMatrixStats(filename: String) : (Int, Int) = {
    var xs = new BufferedReader(new FileReader(filename))

    // header and metadata
    var header = xs.readLine()

    var line = xs.readLine()
    val counts = line.trim().split("\\s+")
    val numDocs = java.lang.Integer.parseInt(counts(0))
    val numTokens = java.lang.Integer.parseInt(counts(1))
    if ((numDocs < 0) || (numTokens < 0)) throw new RuntimeException("Illegal input to readTokenMatrix")

    xs.close()
    return (numDocs, numTokens)
  }

  /* the input file is expected to follow the format:
  *  <header>
  *  <num documents> <num tokens>
  *  <tokenlist>
  *  <document word matrix, where each row repesents a document and each column a distinct token>
  *    each line of the doc word matrix begins with class (0 or 1) and ends with -1
  *    the matrix is sparse, so each row has a tuple of (tokenIndex, number of appearances)
  */
  def readTokenMatrix(filename: String)
     : (Matrix[Int], String, Vector[Int]) = {

    var xs = new BufferedReader(new FileReader(filename))

    // header and metadata
    var header = xs.readLine()

    var line = xs.readLine()
    val counts = line.trim().split("\\s+")
    val numDocs = java.lang.Integer.parseInt(counts(0))
    val numTokens = java.lang.Integer.parseInt(counts(1))
    if ((numDocs < 0) || (numTokens < 0)) throw new RuntimeException("Illegal input to readTokenMatrix")

    // tokens
    val tokenlist = xs.readLine()

    // training data
    //var trainMatrix = Matrix.zeros(numDocs, numTokens).toInt(e => e.asInstanceOf[Int])
    //var trainCategory = Vector.zeros(numDocs).toInt(e => e.asInstanceOf[Int]).trans // col vector

    val trainCatSeq = new ArrayBuffer[Int]()
    for (m <- 0 until numDocs){
      line = xs.readLine()
      line = line.trim()
      val nums = line.split("\\s+")

      trainCatSeq += java.lang.Integer.parseInt(nums(0))
    }
    val trainCategory = Vector.fromSeq(trainCatSeq).trans

    xs.close()
    xs = new BufferedReader(new FileReader(filename))
    xs.readLine(); xs.readLine(); xs.readLine()    
  
    val trainMatSeq = new ArrayBuffer[Vector[Int]]()
    for (m <- 0 until numDocs) {
      line = xs.readLine()
      line = line.trim()
      val nums = line.split("\\s+")

      var row = Vector[Int](numTokens)
      var cumsum = 0; var j = 1
      // this could be vectorized
      while (j < nums.length - 1){
        cumsum += java.lang.Integer.parseInt(nums(j))
        row(cumsum) = java.lang.Integer.parseInt(nums(j+1))
        j += 2
      }
      trainMatSeq += row
    }
    val trainMatrix = Matrix(Vector.fromSeq(trainMatSeq))

//    for (m <- 1 until numDocs){
//      line = xs.readLine()
//      line = line.trim()
//      val nums = line.split("\\s+")
//
//      trainCategory(m) = java.lang.Integer.parseInt(nums(0))
//
//      var cumsum = 0; var j = 1
//      // this could be vectorized
//      while (j < nums.length - 1){
//        cumsum += java.lang.Integer.parseInt(nums(j))
//        trainMatrix(m, cumsum) = java.lang.Integer.parseInt(nums(j+1))
//        j += 2
//      }
//
//    }
    xs.close()

    return (trainMatrix,tokenlist,trainCategory)
  }
}