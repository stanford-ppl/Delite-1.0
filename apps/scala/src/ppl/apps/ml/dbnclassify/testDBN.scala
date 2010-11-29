package ppl.apps.ml.dbnclassify

import ppl.delite.core.appinclude._
//import ppl.apps.ml.io.MLInputReader
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.core.{Delite, DeliteApplication}
import java.io.{BufferedWriter, File, PrintWriter, FileWriter, BufferedReader, FileReader}


object testDBN extends DeliteApplication {

  def run(args: Array[String]) = {

    Delite.init = true
    print("loading data...............")

    //load in all the necessary weights and biases
    val vis_hidW = MLInputReader.read("dbnWeights/vis_hidW")
    print(" . ")
    val vis_bias = MLInputReader.readVector("dbnWeights/vis_bias")
    print(" . ")
    val hid_bias = MLInputReader.readVector("dbnWeights/hid_gen_bias")
    print(" . ")
    val hid_hid2W = MLInputReader.read("dbnWeights/hid_hid2W")
    print(" . ")
    val hid2_bias = MLInputReader.readVector("dbnWeights/hid2_gen_bias")
    print(" . ")
    val hid2_hid3W = MLInputReader.read("dbnWeights/hid2_hid3W")
    print(" . ")
    val hid3_bias_nolabel = MLInputReader.read("dbnWeights/hid3_rec_bias")
    print(" . ")
    val hid3_bias = MLInputReader.readVector("dbnWeights/hid3labels_gen_bias")
    print(" . ")
    val hid3_topW = MLInputReader.read("dbnWeights/hid3labels_topW")
    print(" . ")
    val top_bias = MLInputReader.readVector("dbnWeights/top_bias")
    print(" . ")

    //load in test data and labels
    val test_images = MLInputReader.read("data/ml/mnist/normal-test-digits.ascii")
    print(" . ")
    val test_labels = MLInputReader.readVector("data/ml/mnist/test-labels.ascii")
    print(" . ")

    println("done")
    Delite.init = false

    print("running input through network layers.........")
    //run input through the network
    val hidprobs = (1 + (-( test_images * vis_hidW ) - hid_bias.repmat((test_images.numRows), 1)).exp).reciprocal
    val hidstates = hidprobs > Matrix.rand(hidprobs.numRows, hidprobs.numCols)
    print(" 1..")

    //for debugging
    println(" ")
    for(i <- 0 until hidprobs.numRows){
      if(i % 1000 == 1){
        for(j <- 0 until hidprobs.numCols){
          print(hidprobs(i,j) + " ")
        }
        println(" ")
      }
    }
    println(" ")
      for(i <- 0 until hidstates.numRows){
        if(i % 1000 == 1){
          for(j <- 0 until hidstates.numCols){
            print(hidstates(i,j) + " ")
          }
          println(" ")
        }
      }


    val hid2probs = (1 + (-( /*hidstates*/hidprobs * hid_hid2W ) - hid2_bias.repmat((hidstates.numRows), 1)).exp).reciprocal
    val hid2states = hid2probs > Matrix.rand(hid2probs.numRows, hid2probs.numCols)
    print(" 2..")

    //for debugging
    println(" ")
    for(i <- 0 until hid2probs.numRows){
      if(i % 1000 == 1){
        for(j <- 0 until hid2probs.numCols){
          print(hid2probs(i,j) + " ")
        }
        println(" ")
      }
    }
    println(" ")
      for(i <- 0 until hid2states.numRows){
        if(i % 1000 == 1){
          for(j <- 0 until hid2states.numCols){
            print(hid2states(i,j) + " ")
          }
          println(" ")
        }
      }
    
    val hid3probs = (1 + (-( hid2states * hid2_hid3W ) - hid3_bias_nolabel.repmat((hid2states.numRows), 1)).exp).reciprocal
    val hid3states_nolabel = hid3probs > Matrix.rand(hid3probs.numRows, hid3probs.numCols)
    print(" 3..")

    val blankLabels = Matrix.zeros(hid3states_nolabel.numRows, 10)
    val hid3states = hid3states_nolabel.insertAllCols(hid3states_nolabel.numCols, blankLabels)
    //println("hid3states cols: " + hid3states.numCols + "  nolabels numcols: " + hid3states_nolabel.numCols)

    //for debugging
    println(" ")
    for(i <- 0 until hid3probs.numRows){
      if(i % 1000 == 1){
        for(j <- 0 until hid3probs.numCols){
          print(hid3probs(i,j) + " ")
        }
        println(" ")
      }
    }

    val topProbs = (1 + (-( hid3states * hid3_topW ) - top_bias.repmat((hid3states.numRows), 1)).exp).reciprocal
    val topStates = topProbs > Matrix.rand(topProbs.numRows, topProbs.numCols)
    print(" 4..")
    
    val genHid3Inputs = -topStates * hid3_topW.trans   - hid3_bias.repmat(topStates.numRows, 1)  //should bias be included?
    val guessedLabels = genHid3Inputs.removeCols(0, genHid3Inputs.numCols - 10)
    //println("guessedLabels numCols: " +guessedLabels.numCols + " numrows: "+guessedLabels.numRows)

    //for(num <- 0 until guessedLabels.numCols){
      //print(guessedLabels(0, num) + " ")
    //}

    println("done")

    var numCorrect = 0

    //calculate what dbn classifies guesses as
    for(guessNum <- 0 until guessedLabels.numRows){
      var max = -99999.9
      var guess = -1
      for(num <- 0 until guessedLabels.numCols){
        val thisGuess = guessedLabels(guessNum, num)
        if(thisGuess > max){
          max = thisGuess
          guess = num
        }
      }

//      println("actual label: " + test_labels(guessNum) + " guessed label: " + guess + " max: " + max)
      if(test_labels(guessNum) == guess){
        numCorrect = numCorrect + 1
      }
      
    }

    val percentCorrect = 100.0*(numCorrect*1.0)/(guessedLabels.numRows * 1.0)
    println(" test set error = " + (100.0 - percentCorrect) + "% numCorrect: " + numCorrect + " (out of " +guessedLabels.numRows +")")

    Delite.init = false

  }
}

