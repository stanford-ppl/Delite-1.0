package ppl.apps.tests

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}
import ppl.apps.ml.gda.GDA
import ppl.apps.ml.linreg.LinReg
import ppl.apps.ml.nb.NaiveBayes

class AppSuite extends JUnitSuite {

  @Test def gda() {
    GDA.main(Array("data/ml/gda/q1x.dat", "data/ml/gda/q1y.dat"))
    assert(true)
  }

  @Test def linreg() {
    LinReg.main(Array("data/ml/linreg/q2x.dat", "data/ml/linreg/q2y.dat"))
    assert(true)
  }

  @Test def nb() {
    NaiveBayes.main(Array("data/ml/nb/MATRIX.TRAIN.50", "data/ml/nb/MATRIX.TEST"))
    assert(true)
  }


}