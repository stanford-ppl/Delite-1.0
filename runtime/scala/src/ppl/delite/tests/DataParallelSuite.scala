/* Unit tests for Delite data parallel operations.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 8, 2010
 * modified: Feb 8, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.tests

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import ppl.delite.core.appinclude._

/*
class DataParallelSuite extends JUnitSuite {
  implicit val dblTcProxyFactory: TestCollection.ProxyFactory[Double] = new TestCollection.ProxyFactory[Double]
  implicit def tcBuilder : TestCollection.Builder = new TestCollection.Builder
  
  var tc : TestCollection[Double] = null.asInstanceOf[TestCollection[Double]]
  var f : Double => Double = null.asInstanceOf[Double=>Double]
  var op : DeliteOP_TestMap[Double,Double,TestCollection] = null.asInstanceOf[DeliteOP_TestMap[Double,Double,TestCollection]]

  private val randRef = new java.lang.ThreadLocal[scala.util.Random] {
    override def initialValue = new scala.util.Random(100)
  }

  @Before def initialize() {
    tc = TestCollection[Double](1000)
    for (i <- 0 until tc.size){
      tc(i) = randRef.get.nextDouble*100
    }
    f = a => a*2
    op = DeliteOP_TestMap[Double,Double,TestCollection](tc,f)
  }



  @Test def verifyMapLessChunks() {
    testChunking(10)
  }

  @Test def verifyMapSameChunks() {
    testChunking(1000)
  }

  @Test def verifyMapMoreChunks() {
    testChunking(10000)
  }

  @Test def verifyMapUnevenChunks() {
    testChunking(7)
  }

  def testChunking(num_chunks: Int) {
    val chunks = op.testChunks(num_chunks)
    val chunk_size = Math.ceil(tc.size / num_chunks).asInstanceOf[Int]

    var last_chunk_en = -1
    for (i <- 0 until num_chunks){
      chunks(i).task

      // test boundaries
      val chunk_st = chunks(i).start
      val chunk_en = chunks(i).end
      assert(chunk_st == last_chunk_en+1)
      assert(chunk_en < tc.size)
      last_chunk_en = chunk_en
    }

    // test correct mapping
    val out = op.task
    assert(out.size == tc.size)
    for (i <- 0 until out.size){
      assert(out(i) == f(tc(i)))
    }
  }

  @Test def testSeq(){
    val out = op.seq
    for (i <- 0 until out.size){
      assert(out(i) == f(tc(i)))
    }
  }

}
*/