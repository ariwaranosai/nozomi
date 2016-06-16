package nozomi.nzmlib.optimization

import breeze.linalg.{Vector, Matrix}
import nozomi.util.NZMLogging

/**
  * Created by sai on 16-6-16.
  */

trait FactorizationMachineAlgorithm[M <: FactorizationMachineModel]
  extends NZMLogging {
  //
  protected def createModel(w_0: Double, w: Vector[Double], v: Matrix[Double]): M

  def run(data: Seq[Vector[Double]]): M

}


class FactoriaztionMachinewithSGD[M <: FactorizationMachineModel](
                                                                 learn_rate: Double,
                                                                 iter_num: Int
                                                                 )
  extends FactorizationMachineAlgorithm[M] {
  //
  override protected def createModel(w_0: Double, w: Vector[Double], v: Matrix[Double]): M = ???
  protected def SGD() = ???

  override def run(data: Seq[Vector[Double]]): M = ???

  // deal with both regression and classification
  var delta: (Double, Double) => Double = ???

}