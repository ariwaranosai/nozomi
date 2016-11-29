package nozomi.nzmlib.nmf

import breeze.linalg.{DenseMatrix, Matrix}
import nozomi.util.NZMLogging

/**
  * Created by ariwaranosai on 2016/11/29.
  *
  */

abstract class NMF(X: Matrix[Double], UV: Array[Matrix[Double]]) {
}

trait NMFAlgorithm[T <: NMF] {
  def optimizer(x :DenseMatrix[Double]): Array[DenseMatrix[Double]]
}

/** Ordinary NMF
  *
  * X = UV'
  *
  * @param k number of hidden factors
  */
class OrdinaryNMF(k: Int, RX: Matrix[Double]) extends NMFAlgorithm[NMF] with NZMLogging {self =>
  var repeat = 1000
  var U: Option[DenseMatrix[Double]] = None
  var V: Option[DenseMatrix[Double]] = None

  def randomInitU(): Unit = {
    U = Some(DenseMatrix.rand(RX.rows, k))
  }

  def randomInitV(): Unit = {
    V = Some(DenseMatrix.rand(RX.cols, k))
  }

  override def optimizer(X: DenseMatrix[Double]): Array[DenseMatrix[Double]] = {

    U match {
      case None => randomInitU()
      case _ =>
    }

    V match {
      case None => randomInitV()
      case _ =>
    }

    // X => m * n
    // X = UV' U => m * k V' => n * k
    // update V
    var XU: DenseMatrix[Double] = X.t * U.get // n * m * m * k
    var UU: DenseMatrix[Double] = U.get.t * U.get // k * m * m * k
    var VUU: DenseMatrix[Double] = V.get * UU // n * k * k * k

    V = Some(V.get :* (XU :/ VUU.map(x => math.max(x, 1e-10))))

    // update U
    var XV: DenseMatrix[Double] = X * V.get
    var VV: DenseMatrix[Double] = V.get.t * V.get
    var UVV: DenseMatrix[Double] = U.get * VV

    U = Some(U.get :* (XV :/ UVV.map(x => math.max(x, 1e-10))))

    (0 until repeat) foreach { _ =>
      // update V
      XU = X.t * U.get
      UU = U.get.t * U.get
      VUU = V.get * UU

      V = Some(V.get :* (XU :/ VUU.map(x => math.max(x, 1e-10))))

      // update U
      XV = X * V.get
      VV = V.get.t * V.get
      UVV = U.get * VV
      U = Some(U.get :* (XV :/ UVV.map(x => math.max(x, 1e-10))))
    }

    Array(U.get, V.get)
  }
}
