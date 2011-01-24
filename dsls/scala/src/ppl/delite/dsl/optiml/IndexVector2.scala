package ppl.delite.dsl.optiml

import ppl.delite.dsl.optiml.specialized.IntVectorImpl
import ppl.delite.core.{DeliteFunc, DeliteProxyFactory}
import ppl.delite.dsl.optiml.Matrix

class IndexVector2  {
  def indRow = _indRow
  def indCol = _indCol

  private var _indRow : IndexVector = null
  private var _indCol : IndexVector = null

  def this(rows: IndexVector, cols: IndexVector) = {
    this()

    // _indRow means this IndexVector represents the rows of a Matrix, so it should be stored as a column vector
    _indRow = rows
    if (_indRow.is_row)  _indRow = _indRow.mtrans.asInstanceOf[IndexVector]
    _indCol = cols
    if (!_indCol.is_row) _indCol = _indCol.mtrans.asInstanceOf[IndexVector]
  }

  /**
   * Matrix construction
   */
  def apply[@specialized(Double)A](block : Int => Vector[A])(implicit pFact : DeliteProxyFactory[Matrix[A]],
                                                                      vecPFact: DeliteProxyFactory[Vector[Vector[A]]], c: ClassManifest[A]) : Matrix[A] = {

    (_indRow, _indCol) match {
      case (wc: IndexVectorWC, ind: IndexVector) => constructMatrix(ind)(block)
      case (ind: IndexVector, wc: IndexVectorWC) => constructMatrix(ind)(block)
      case _ => throw new UnsupportedOperationException("illegal matrix construction")
    }
  }

  def apply[@specialized(Double)A](block : (Int,Int) => A)(implicit pFact : DeliteProxyFactory[Matrix[A]], vecPFact: DeliteProxyFactory[Vector[A]],
                                                           vecVecPFact: DeliteProxyFactory[Vector[Vector[A]]], c: ClassManifest[A]) : Matrix[A] = {
    if (_indRow.isInstanceOf[IndexVectorWC] || _indCol.isInstanceOf[IndexVectorWC]) {
      throw new UnsupportedOperationException("illegal matrix construction")
    }

    constructMatrix(_indRow, _indCol)(block)
  }

  def constructMatrix[@specialized(Double)A](ind: IndexVector)(block: Int => Vector[A])
                                            (implicit vecPFact: DeliteProxyFactory[Vector[Vector[A]]], pFact: DeliteProxyFactory[Matrix[A]], c: ClassManifest[A]) : Matrix[A] = {
    val deps = block match {
      case f: DeliteFunc => f.deps
      case _ => Seq()
    }

    Matrix[A](ind.map[Vector[A]](DeliteFunc(j => block(j), deps: _*)))
  }

  def constructMatrix[@specialized(Double)A](indRow: IndexVector, indCol: IndexVector)(block: (Int,Int) => A)
                                            (implicit vecPFact: DeliteProxyFactory[Vector[A]], vecVecPFact: DeliteProxyFactory[Vector[Vector[A]]],
                                             pFact: DeliteProxyFactory[Matrix[A]], c: ClassManifest[A]): Matrix[A] = {
    val deps = block match {
      case f: DeliteFunc => f.deps
      case _ => Seq()
    }

    Matrix[A](indRow.map[Vector[A]]( DeliteFunc(i => indCol.map[A](j => block(i,j)), deps: _*)))
  }


}
