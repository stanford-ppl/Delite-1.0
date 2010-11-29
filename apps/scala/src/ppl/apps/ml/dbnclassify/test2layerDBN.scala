package ppl.apps.ml.dbnclassify

/**
 * Created by IntelliJ IDEA.
 * User: joe
 * Date: Aug 16, 2010
 * Time: 12:22:49 PM
 * To change this template use File | Settings | File Templates.
 */

import ppl.delite.core.appinclude._
//import ppl.apps.ml.io.MLInputReader
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.core.{Delite, DeliteApplication}
import java.io.{BufferedWriter, File, PrintWriter, FileWriter, BufferedReader, FileReader}


object test2layerDBN extends DeliteApplication {

  val weightDir = "rfDBNWeights/"
  val dataDir = "data/ml/mnist/"

  def run(args: Array[String]) = {

    Delite.init = true
    print("loading data...............")

    //load in all the necessary weights and biases
    val vis_hidW = MLInputReader.read(weightDir + "vis_hidW")
    print(" . ")
    val vis_bias = MLInputReader.readVector(weightDir + "vis_bias")
    print(" . ")
    val hid_bias = MLInputReader.readVector(weightDir + "hid_gen_bias")
    print(" . ")
    val hid_hid2W = MLInputReader.read(weightDir + "hid_hid2W")
    print(" . ")
    val hid2_bias = MLInputReader.readVector(weightDir + "hid2labels_gen_bias")
    print(" . ")
    val hid2_bias_nolabel = MLInputReader.readVector(weightDir + "hid2_rec_bias")
    print(" . ")
    val hid2_topW = MLInputReader.read(weightDir + "hid2labels_topW")
    print(" . ")
    val top_bias = MLInputReader.readVector(weightDir + "top_bias")
    print(" . ")

    //load in test data and labels
    val test_images = MLInputReader.read(dataDir + "normal-test-digits.ascii")
    print(" . ")
    val test_labels = MLInputReader.readVector(dataDir + "test-labels.ascii")
    print(" . ")

    println("done")
//    Delite.init = false

    print("running input through network layers.........")
    //run input through the network
    val hidprobs = (1 + (-(  test_images * vis_hidW ) - hid_bias.repmat((test_images.numRows), 1)).exp).reciprocal
    val hidstates = hidprobs > Matrix.rand(hidprobs.numRows, hidprobs.numCols)
    print(" 1..")

    //for debugging
    /*println(" ")
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
      }*/


    val hid2probs = (1 + (-( /*hidstates*/  hidprobs * hid_hid2W  ) - hid2_bias_nolabel.repmat((hidstates.numRows), 1)).exp).reciprocal
    val hid2states = hid2probs > Matrix.rand(hid2probs.numRows, hid2probs.numCols)
    print(" 2..")

    //for debugging
    /*println(" ")
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
    }*/

    val blankLabels = Matrix.zeros(hid2probs.numRows, 10)
    val hid2states_labels = /*hid2states*/ hid2probs.insertAllCols(hid2states.numCols, blankLabels)




    val useFreeEnergy = true
    //this option classifies the digits by trying out each digit and choosing the one which minimizes the free energy
    //of each test example
    //TODO: verify that this works, and all matrix operations are safe
    if(useFreeEnergy){

      println()
      println()
      print("Calculating free energies for label ")
      var negFreeEnergies = Matrix.zeros(hid2states_labels.numRows, 10)

      for(guessNum <- 0 until 10){
        print(guessNum + "... ")
        changeClass(hid2states_labels, guessNum)

        val biasProd = hid2states_labels * hid2_bias.trans
        val topTerm = ( hid2states_labels * hid2_topW + top_bias.repmat(hid2states_labels.numRows, 1) ).map(e => scala.math.log(1 + scala.math.exp(e)))
        val topSums = topTerm.sumRow

//        println()
//        println("topsums dims: " + topSums.length + "   biasProds dims: " + biasProd.length)

        var rowIdx = 0
        while(rowIdx < topSums.length){
          negFreeEnergies(rowIdx, guessNum) = biasProd(rowIdx) + topSums(rowIdx)
          rowIdx += 1
        }

      }


      var numCorrect = 0
      //calculate what dbn classifies guesses as
      for(guessNum <- 0 until negFreeEnergies.numRows){
        var max = -99999.9
        var guess = -1
        for(num <- 0 until negFreeEnergies.numCols){
          val thisGuess = negFreeEnergies(guessNum, num)
          if(thisGuess > max){
            max = thisGuess
            guess = num
          }
        }

  //      println("actual label: " + test_labels(guessNum) + " guessed label: " + guess + " max: " + max)
        if(test_labels(guessNum) == guess) numCorrect += 1
      }

      val percentCorrect = 100.0*(numCorrect*1.0)/(negFreeEnergies.numRows * 1.0)
      println(" test set error = " + (100.0 - percentCorrect) + "% numCorrect: " + numCorrect + " (out of " +negFreeEnergies.numRows +")")

    } //end classification using free energy
    else{ //otherwise, classify using a gibbs sampling-like technique that
          //gets the digit unit with the highest probability of being activated after 1 gibbs samples

      var topProbs = (1 + (-( hid2states_labels * hid2_topW ) - top_bias.repmat((hid2states_labels.numRows), 1)).exp).reciprocal
      var topStates = topProbs > Matrix.rand(topProbs.numRows, topProbs.numCols)
      print(" 3..")

      var genHid2Probs = (1 + ( - (/*topStates*/ topProbs * hid2_topW.trans) /*- hid2_bias.repmat(topStates.numRows, 1)*/).exp).reciprocal
      val guessedLabels = genHid2Probs.removeCols(0, genHid2Probs.numCols - 10)

      //for debugging
      println(" ")
      for(i <- 0 until guessedLabels.numRows){
        if(i % 1000 == 1){
          for(j <- 0 until guessedLabels.numCols){
            print(guessedLabels(i,j) + " ")
          }
          println(" ")
        }
      }

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


    } //end classification

  }

  def changeClass(m: Matrix[Double],  num: Int){
    val start = m.numCols - 10

    for(row <- 0 until m.numRows){

      var idx = 0
      while(idx < 10){
        if(idx == num) m(row, start+idx) = 1
        else m(row, start+idx) = 0
        idx += 1
      }
    }
  }
}
