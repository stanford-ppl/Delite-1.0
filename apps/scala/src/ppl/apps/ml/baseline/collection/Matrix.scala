package ppl.apps.ml.baseline.collection

abstract class Matrix {

  def width: Int

  // todo: is it worth optimizing by removing the calculation
  def height: Int  = size / width

  def size: Int

  private[collection] def chkBounds(m1: Matrix, m2:Matrix) {
    if(m1.width != m2.width || m1.height != m2.height)
      throw new IllegalArgumentException("Matrix dimensions need to agree")
  }

  private[collection] def chkBounds(m: Matrix, v: Vector) {
    if (m.width != v.length) throw new IllegalArgumentException("Vector length must equal matrix width")
  }

  private[collection] def chkBounds(i: Int) {
    if (i < 0 || i >= height) throw new IllegalArgumentException("Matrix index out of bounds")
  }

  private[collection] def chkBounds(i: Int, j: Int) {
    if (i < 0 || j < 0 || i >= height || j >= width) throw new IllegalArgumentException("Matrix index out of bounds")
  }

  protected def chkRange(begin: Int, end: Int) {
    if (begin < 0 || end < begin || end > height) throw new IndexOutOfBoundsException
  }


}