package ppl.delite.tests

import ppl.delite.core.DeliteApplication
import org.scalatest.junit.JUnitSuite
import org.junit.{After, Before}

object DeliteStarter extends DeliteApplication {
  def run(args: Array[String]) = {}
}

trait DeliteTestSuite extends JUnitSuite {
  @Before def delite_initialize() {
    DeliteStarter.initialize
  }

  @After def delite_shutdown() {
    DeliteStarter.shutdown
  }  
}