package ppl.apps.ml.tica

import ppl.delite.dsl.optiml._
import ppl.delite.core.{Delite, DeliteApplication}
import ppl.delite.core.appinclude._
import ppl.delite.metrics.PerformanceTimer

/**
 * Created by IntelliJ IDEA.
 * User: joe
 * Date: Aug 3, 2010
 * Time: 2:47:54 PM
 * To change this template use File | Settings | File Templates.
 */

object tester extends DeliteApplication {

  def run(args: Array[String]) = {
    
    Delite.init = true
//    Delite.init = false

    println("here1")
    //val rowI = Vector[Int](1, 3, 4, 5, 6, 9, 10)
    //val colI = Vector[Int](1, 3, 4, 6, 3, 5, 8)
    val rowI = Vector[Int](0, 2, 3, 4, 5, 8, 9)
    val colI = Vector[Int](0, 2, 3, 5, 2, 4, 7)
    val vals = Vector[Double](69, 69, 69, 234, 25, 2456)
    var test = SparseMatrix.fromVectors(15, 10, rowI, colI, vals)

//    var lol = Matrix.zeros(5, 10)
//    lol(4,4) = 7
//    lol(4,7) = 100
//    lol.pprint
//    var test = SparseMatrix.fromDoubleMatrix(lol)

    test(3, 2) = 5.1
    test(1, 1) = 2
    test(1, 3) = 1094
    test(1, 2) = 23890
    test(0,0) = 69
    test(14,9) = 248
    test(13,8) = 23

    test.pprint
    val out = test * test.trans
    println("")
    println("=================sparse matrix mult==================")
    out.pprint
    /*
    println("-==============begin cleanup test===================")
    test(0,0) = 0.0
    test(1,1) = 0.0
    test(13,8) = 0.0
    test.pprint
    println(".........running cleanup")
    test.cleanup
    test.pprint
    test.debug
    test.debugCSC
    */
    /*println("=============begin plusEqualsNNZ test===============")
    val ta = Matrix.ones(15, 10)
    test.plusEqualsNNZOnly(ta)
    test.pprint
    test.debugCSC*/

    /*test.debugCSC
    println("===========begin CSC mult-test=============")
    test *= 2.0
    test.debugCSC
    test.pprint*/

    /*test = test.mapNNZ(e => if(e > 1000.0) 12345 else e)
    println("---------------mapNNZ test----------------")
    test.pprint*/

    //var asdf = SparseMatrix.zeros(10, 8)
    //asdf(2,5) = 1234
    //asdf.pprint

    /*val m = Matrix.randn(10,10)
    val c = m*2.0
    c.pprint

    println("------------begin cutoffAndReturn test---------------------")
    val numZ = c.cutoffAndReturn(.1)
    c.pprint
    println("---------- numZ --------------------")
    numZ.pprint*/


    // RBM method testing stuff
   /* val p = c.reciprocal
    p.pprint
    val s = p.sumCol
    s.pprint
    val q = s.repmat(5, 2)
    q.pprint*/



   /* val temp = Matrix.ones(15, 10)
    val test1 = test + 1.0
    val test2 = test - 2.0
    val test3 = test > 4.0
    val test4 = test < 3.0
    println("================begin toMatrix test1 ==========================")
    test1.pprint

    println("================begin toMatrix test2 ==========================")
    test2.pprint

    println("================begin toMatrix test3 ==========================")
    test3.pprint

    println("================begin toMatrix test4 ==========================")
    test4.pprint */

    /* Dense + Sparse Ops
    val temp = Matrix.ones(15, 10)
    val test1 = temp + test
    val test2 = temp - test
    val test3 = temp dot test
    val test4 = temp < test

    test1.pprint
    test2.pprint
    test3.pprint
    test4.pprint
    */
    //test.debug
    //test.debugCSC

   /* var mult1 = SparseMatrix.zeros(256, 256)
    for (x <- 0 until 255){
      mult1(x, x+1) = 1
      mult1(x+1, x) = 1
      mult1(x, x) = 1
    }
    println(mult1.numElems)
    println(mult1.size)
    //mult1.pprint
    var regular = mult1.toMatrix
    val densematrix = Matrix.randn(256, 256)
    var out = mult1 * densematrix

    PerformanceTimer.start("sparse mult")
    for (x <- 1 until 10000)
      out = mult1 * densematrix
    PerformanceTimer.stop("sparse mult")
    PerformanceTimer.print("sparse mult")

    PerformanceTimer.start("regular mult")
    for (x <- 1 until 10000)
      out = regular * densematrix    
    PerformanceTimer.stop("regular mult")
    PerformanceTimer.print("regular mult") 

    exit(-1)
    
    var mult2 = SparseMatrix.zeros(50,50)
    mult2(0,1) = 2
    mult2(0,2) = 2
    mult2(1,0) = 1
    mult2(1,1) = 1
    mult2(1,2) = 2
*/
    //mult1.pprint
    //mult2.pprint

//    Delite.init = false

    /*
    /*println("==============begin selfTrans test==================")
    test.selfTrans
    println("test nrows and ncols: "+test.numRows + " " + test.numCols )
    test.pprint*/
    */
    /*println("================begin toMatrix test ==========================")
    val mt = test.toMatrix
    mt.pprint*/

    /*println("===================begin Sparse-Sparse Matrix mult test ======================")
    val mtest = mult1*mult2
    mtest.pprint*/

    /*println("==================begin Dense-Sparse Matrix multiplication test ===================")
    val multTest = Matrix.ones(5, 15) * test
    multTest.pprint*/

    /*//TODO: figure out why this freezes with init = false
    println("================begin scalar mult-test===================")
    val m = test * 2.0
    m.pprint
    println(" division of test by 20.....")
    test /= 20
    test.pprint
    println(" multiplication of test by 40 ..... (should be same as m)")
    test *= 40
    test.pprint*/


    /*println("=============begin scalar add-test=======================")
    val add = test - 3.0
    add.pprint*/

    /*println("=====================begin clonetest==============================")
    var cloned = test.clone
    cloned(0,0) = 34588940
    test.pprint
    println("cloned ------>>>>>>>")
    cloned.pprint*/
    /*
    /*println("====================begin trans test================================")
    var trans = test.trans
    trans.pprint*/
    */
    /*println("======================begin toDoubleMatrix test========================")
    var another = test.toDoubleMatrix
    another.pprint*/

//    val addtest = test dot Matrix.ones(15,10)
//    val multTest = test * Matrix.ones(10, 5)
//    Delite.init = false

//    val multTest = test * Vector.ones(10)

//        Delite.init = true

//    val cmpTest1 = test < Matrix.ones(15, 10)
//    val cmpTest2 = test > Matrix.ones(15, 10)
//    val cmpTest3 = test > Matrix.zeros(15, 10)
    //val cmpTest4 = test < Matrix.zeros(15, 10)

//    println("====================================begin addtest==================================")


//    multTest.pprint
//    addtest.pprint
    //cmpTest1.pprint
//    cmpTest3.pprint

    /*PerformanceTimer.start("SM")
    val lol = makeNeighborSparseMatrix(30, 30, 1)
    PerformanceTimer.stop("SM")
    PerformanceTimer.print("SM")

    PerformanceTimer.start("m")
    val m = makeNeighborMatrix(30, 30, 1)
    PerformanceTimer.stop("m")
    PerformanceTimer.print("m")*/

    //////////////////////////////////////////////end testing code
  }
}