package ppl.apps.ml.dbnclassify

import ppl.delite.core.appinclude._
import collection.mutable.ArrayBuffer
//import ppl.apps.ml.io.MLInputReader
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.core.{Delite, DeliteApplication}
import java.io.{BufferedWriter, File, PrintWriter, FileWriter, BufferedReader, FileReader}


object trainDBN extends DeliteApplication {

  val dataDir = "data/ml/mnist/"
  val saveDir = "dbnWeights/"
  val useSparse = true
  val useSparseWeights = false
  val useRF = false

  def run(args: Array[String]) = {
    //This only needs to be done once!
    Delite.init = true

    /*mixUpTrainingData()
    convertLabelsToLabelMatrix(dataDir + "mixed-train-labels.ascii", dataDir + "train-label-matrix.ascii")
    convertLabelsToLabelMatrix(dataDir + "test-labels.ascii", dataDir + "test-label-matrix.ascii")
    normalizePixelValues(dataDir + "mixed-train-digits.ascii", dataDir + "normal-train-digits.ascii")
    normalizePixelValues(dataDir + "test-digits.ascii", dataDir + "normal-test-digits.ascii")
*/
    Delite.init = false

    val inputArgs = new Array[String](9)

    println("Training level 1..........")
    inputArgs(0) = dataDir + "normal-train-digits.ascii"
    inputArgs(1) = saveDir + "vis_hidW"
    inputArgs(2) = saveDir + "vis_bias"
    inputArgs(3) = saveDir + "hid_rec_bias"
    inputArgs(4) = "784" //num visible neurons
    inputArgs(5) = "500" //num hidden neurons
    inputArgs(6) = saveDir
    if(useRF){
      inputArgs(5) = inputArgs(4) //constrain num hidden = num visible
      inputArgs(7) = "28" //x-dim of image
      inputArgs(8) = "28"
      RF_RBM.run(inputArgs)
    }
    else if(useSparseWeights && useSparse)
      swSparseRBM.run(inputArgs)
    else if(useSparseWeights)
      SparseWeightRBM.run(inputArgs)
    else if(useSparse)
      SparseRBM.run(inputArgs)
    else
      RBM.run(inputArgs)


    /*println("Training level 2..........")
    inputArgs(0) = saveDir + "LowerLayerTrain"
    inputArgs(1) = saveDir + "hid_hid2W"
    inputArgs(2) = saveDir + "hid_gen_bias"
    inputArgs(3) = saveDir + "hid2_rec_bias"
    if(useRF)
      inputArgs(4) = "784" //num visible neurons
    else
      inputArgs(4) = "500"

    inputArgs(5) = "500" //num hidden neurons
    inputArgs(6) = saveDir

    if(useSparseWeights && useSparse)
      swSparseRBM.run(inputArgs)
    else if(useSparseWeights)
      SparseWeightRBM.run(inputArgs)
    else if(useSparse)
      SparseRBM.run(inputArgs)
    else
      RBM.run(inputArgs)*/

    /*println("Training level 3..........")
    inputArgs(0) = saveDir + "LowerLayerTrain"
    inputArgs(1) = saveDir + "hid2_hid3W"
    inputArgs(2) = saveDir + "hid2_gen_bias"
    inputArgs(3) = saveDir + "hid3_rec_bias"
    inputArgs(4) = "750" //num visible neurons
    inputArgs(5) = "500" //num hidden neurons
    if(useSparseWeights)
      SparseWeightRBM.run(inputArgs)
    else if(useSparse)
      SparseRBM.run(inputArgs)
    else
      RBM.run(inputArgs)*/

    //this adds the labels of the digits onto the end of each row of the training data
    //which allows the dbn to "learn" the labels of the digits
    /*appendLineByLine(saveDir + "/LowerLayerTrain", dataDir + "train-label-matrix.ascii", saveDir + "/TopLayerLabelTrain")

    println("Training final level with output from layer 2 and labels..........")
    inputArgs(0) = saveDir + "TopLayerLabelTrain"
    inputArgs(1) = saveDir + "hid2labels_topW"
    inputArgs(2) = saveDir + "hid2labels_gen_bias"
    inputArgs(3) = saveDir + "top_bias"
    inputArgs(4) = "510" //num visible neurons
    inputArgs(5) = "1000" //num hidden neurons
    inputArgs(6) = saveDir
    if(useSparseWeights && useSparse)
      swSparseRBM.run(inputArgs)
    else if(useSparseWeights)
      SparseWeightRBM.run(inputArgs)
    else if(useSparse)
      SparseRBM.run(inputArgs)
    else
      RBM.run(inputArgs)*/
  }

  /*
  This function mixes up the mnist data so each training batch won't have just one class
  of digits in it
   */
  def mixUpTrainingData() = {
    //mix up training data first
    try{
      var outTrainLabels = new File(dataDir + "mixed-train-labels.ascii")
      var outTrainDigits = new File(dataDir + "mixed-train-digits.ascii")

      if(outTrainLabels.exists()) {
        println("this file exists! ..deleting file")
        outTrainLabels.delete()
      }
      if(outTrainDigits.exists()) {
        println("this file exists! ..deleting file")
        outTrainDigits.delete()
      }

      val labelWriter = new PrintWriter(new BufferedWriter(new FileWriter(outTrainLabels, true)))
      val digitWriter = new PrintWriter(new BufferedWriter(new FileWriter(outTrainDigits, true)))

      val trainLabels = MLInputReader.read(dataDir + "train-labels.ascii")
      val trainDigits = MLInputReader.read(dataDir + "train-digits.ascii")

      var nums = new ArrayBuffer[Int]()

      var idx = 0
      while(idx < (trainDigits.numRows/10)){
        nums.append(idx)
        idx += 1
      }

      idx = 0
      while(idx < (trainLabels.numRows/10)){
        val pos = (scala.math.random*nums.size).toInt //position to remove
        val rand = nums(pos) //the random value still left to pick from the training data

        var innerIdx = 0
        while(innerIdx < 10){

          val labelToWrite = trainLabels((innerIdx*trainDigits.numRows/10) + rand, 0).toInt
          labelWriter.write(labelToWrite + " ")
          labelWriter.println()

          writeRow(digitWriter, trainDigits, (innerIdx*trainDigits.numRows/10) + rand)
          innerIdx += 1
        }

        nums.remove(pos)
        idx += 1
      }
      
      labelWriter.close()
      digitWriter.close()
    }
    catch {
      case e: Exception => println("Unable to save the matrix!")
    }
  }

  def writeRow(writer : PrintWriter, data : Matrix[Double], row : Int) = {
    var idx = 0
    while(idx < data.numCols){
      writer.write(data(row, idx) + " ")
      idx += 1
    }
    writer.println()
  }

  /*
  this converts a file with a digit label each line, like:
  0
  2
  4
  1

  into a label matrix with rows 10 wide. the above file will be converted to:
  1 0 0 0 0 0 0 0 0 0
  0 0 1 0 0 0 0 0 0 0
  0 0 0 0 1 0 0 0 0 0
  0 1 0 0 0 0 0 0 0 0


  */
  def convertLabelsToLabelMatrix(input: String, output: String) = {
    try{
      var outfile = new File(output)
      if(outfile.exists()) {
        println("this file exists! ..deleting file")
        outfile.delete()
      }
      var infile = new File(input)

      val reader = new BufferedReader(new FileReader(infile))
      val writer = new PrintWriter(new BufferedWriter(new FileWriter(outfile, true)))

      var line = reader.readLine()
      while(line != null){
        line = line.trim
        val label = Integer.parseInt(line).intValue()
        for(num <- 0 until 10 ) {
          if(num == label) writer.print("1 ")
          else writer.print("0 ")
          
        }
        line = reader.readLine()
        writer.println()
      }
      writer.close()
      reader.close()
    }
    catch {
      case e: Exception => println("Unable to save the matrix to " + output + "!")
    }

  }

  /*
  this function normalizes pixel values (it assumes all pixel values are out of 256)
  */
  def normalizePixelValues(input: String, output: String) = {
    try{
      var outfile = new File(output)
      if(outfile.exists()) {
        println("this file exists! ..deleting file")
        outfile.delete()
      }
      var infile = new File(input)

      val reader = new BufferedReader(new FileReader(infile))
      val writer = new PrintWriter(new BufferedWriter(new FileWriter(outfile, true)))

      var line = reader.readLine()
      line = line.trim()
      var dbls = line.split("\\s+")

      while (line != null){
        for (i <- 0 until dbls.length){
          writer.print( (java.lang.Double.parseDouble(dbls(i)) / 256.0) + " ")
        }

        line = reader.readLine()
        if (line != null) {
          line = line.trim()
          dbls = line.split("\\s+")
          writer.println()
        }
      }

      writer.close()
      reader.close()
    }
    catch {
      case e: Exception => println("Unable to save the matrix to " + output + "!")
    }

  }

  /* this function appends each line of the last file to each line of the corresponding first file, 
  for as many lines as first has. 
  */
  def appendLineByLine(first: String, last: String, out: String) = {
    try{
      var outfile = new File(out)
      if(outfile.exists()) {
        println("this file exists! ..deleting file")
        outfile.delete()
      }
      var firstfile = new File(first)
      var lastfile = new File(last)

      val firstreader = new BufferedReader(new FileReader(firstfile))
      val lastreader = new BufferedReader(new FileReader(lastfile))
      val writer = new PrintWriter(new BufferedWriter(new FileWriter(outfile, true)))

      var firstline = firstreader.readLine()
      var lastline = lastreader.readLine()

      while(firstline != null){
        if(lastline != null){
          writer.println(firstline + " " + lastline)
          lastline = lastreader.readLine()
        }
        else
          writer.println(firstline)
        
        firstline = firstreader.readLine()
      }

      writer.close()
      firstreader.close()
      lastreader.close()
    }
    catch {
      case e: Exception => println("Unable to save the matrix to " + out + "!")
    }
  }

}
