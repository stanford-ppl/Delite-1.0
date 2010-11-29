package ppl.apps.ml.baseline.collection


import ppl.apps.ml.baseline.collection.{DoubleVector, BooleanVector, DoubleMatrix}
import java.io.{FileReader, BufferedReader}
import ppl.delite.nativeGPU.DoubleArray

object MLInputReader {

  implicit def doubleToBoolean(d: Double): Boolean = if (d <= 0) false else true

  /**
   * Reads from a file whose format is m lines with n floats per line
   * Each float is separated by whitespace
   *
   */
  def readDoubleMatrix(filename: String): DoubleMatrix = {

    val file = new BufferedReader(new FileReader(filename))

    //first determine the width of the vector by loading the first line
    var line = file.readLine
    assert(line != null)
    val v = readDoubleVectorFromLine(line)
    val m = DoubleMatrix(0,v.length)
    m ::= v

    //load the rest of the line
    line = file.readLine
    while(line != null) {
      m ::= readDoubleVectorFromLine(line)
      line = file.readLine
    }
    file.close
    m
  }

  /**
   * Reads from a file whose format is m lines with n floats per line
   * Each float is separated by whitespace
   *
   */
  def readFloatMatrix(filename: String): FloatMatrix = {

    val file = new BufferedReader(new FileReader(filename))

    //first determine the width of the vector by loading the first line
    var line = file.readLine
    assert(line != null)
    val v = readFloatVectorFromLine(line)
    val m = FloatMatrix(0,v.length)
    m ::= v

    //load the rest of the line
    line = file.readLine
    while(line != null) {
      m ::= readFloatVectorFromLine(line)
      line = file.readLine
    }
    file.close
    m
  }

  private def readDoubleVectorFromLine(l: String): DoubleVector = {
    val line = l.trim
    val dbls = line.split("\\s+")

    val v = DoubleVector(dbls.length)
    for(i <- 0 until dbls.length) {
      v(i) = java.lang.Double.parseDouble(dbls(i))
    }
    return v
  }

  private def readFloatVectorFromLine(l: String): FloatVector = {
    val line = l.trim
    val flts = line.split("\\s+")

    val v = FloatVector(flts.length)
    for(i <- 0 until flts.length) {
      v(i) = java.lang.Float.parseFloat(flts(i))
    }
    return v
  }

  def readDoubleVector(filename: String): DoubleVector = {
    val v = DoubleVector(0)
    val file = new BufferedReader(new FileReader(filename))
    var line = file.readLine
    while(line != null) {
      line = line.trim
      val dbl = java.lang.Double.parseDouble(line)
      v append dbl
      line = file.readLine
    }
    file.close
    v
  }

  def readBooleanVector(filename: String): BooleanVector = {
    val v = BooleanVector(0)

    val file = new BufferedReader(new FileReader(filename))
    var line = file.readLine
    while(line != null) {
      line = line.trim
      val dbl = java.lang.Double.parseDouble(line)      
      if (dbl <= 0.0) {
        v += false
      }
      else {        
        v += true
      }
      line = file.readLine
    }

    file.close
    v
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
  def readTokenMatrix(filename: String): (DoubleMatrix, String, DoubleVector) = {

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

    val trainCatSeq = new Array[Double](numDocs)
    for (m <- 0 until numDocs){
      line = xs.readLine()
      line = line.trim()
      val nums = line.split("\\s+")

      trainCatSeq(m) = java.lang.Integer.parseInt(nums(0))
    }
    val trainCategory = DoubleVector(trainCatSeq, false, numDocs)

    xs.close()
    xs = new BufferedReader(new FileReader(filename))
    xs.readLine(); xs.readLine(); xs.readLine()

    val trainMatrix = DoubleMatrix(0, numTokens)
    for (m <- 0 until numDocs) {
      line = xs.readLine()
      line = line.trim()
      val nums = line.split("\\s+")

      var row = DoubleVector(numTokens)
      var cumsum = 0; var j = 1
      // this could be vectorized
      while (j < nums.length - 1){
        cumsum += java.lang.Integer.parseInt(nums(j))
        row(cumsum) = java.lang.Integer.parseInt(nums(j+1))
        j += 2
      }
      trainMatrix ::= row
    }

    xs.close()

    return (trainMatrix,tokenlist,trainCategory)
  }

}