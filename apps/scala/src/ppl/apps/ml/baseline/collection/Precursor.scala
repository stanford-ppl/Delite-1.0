package ppl.apps.ml.baseline.collection

object Precursor {
  implicit def intToMatrixOps(i:Int) = new {
    def +(m: DoubleMatrix) = m + i
    def +(m: FloatMatrix)  = m + i
    def *(m: DoubleMatrix) = m * i
    def *(m: FloatMatrix)  = m * i
    def *(v: DoubleVector) = v * i
  }

  implicit def floatToMatrixOps(i:Float) = new {
    def +(m: DoubleMatrix) = m + i
    def +(m: FloatMatrix)  = m + i
    def *(m: DoubleMatrix) = m * i
    def *(m: FloatMatrix)  = m * i
    def *(v: DoubleVector) = v * i
  }

  implicit def floatToMatrixOps(i:Double) = new {
    def +(m: DoubleMatrix) = m + i
    def *(m: DoubleMatrix) = m * i
    def *(v: DoubleVector) = v * i

  }
}