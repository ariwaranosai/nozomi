package nozomi.nzmlib.optimization

import breeze.linalg.DenseVector

/**
  * Created by ariwaranosai on 16/3/2.
  *
  */

abstract class Updater {

    /**
      * Compute an updated value for weights given the gradient, stepSize, iter and regularization
      * parameter. Returns the regularizaiton value with updated weights.
      * @param weightsOld  Old weights 1 * d
      * @param gradient Gradient  1 * d
      * @param stepSize Step size every iteration \alpha
      * @param iter Iteration number
      * @param regParam regularization parameter
      * @return (updated weights, regularization value)
      */
    def compute(weightsOld: DenseVector[Double],
                gradient: DenseVector[Double],
                stepSize: Double,
                iter: Int,
                regParam: Double): (DenseVector[Double], Double)

}


class SimpleUpdater extends Updater {
    /**
      * Compute an updated value for weights given the gradient, stepSize, iter and regularization
      * parameter. Returns the regularizaiton value with updated weights.
      *
      * @param weightsOld Old weights 1 * d
      * @param gradient   Gradient  1 * d
      * @param stepSize   Step size every iteration \alpha
      * @param iter       Iteration number
      * @param regParam   regularization parameter
      * @return (updated weights, regularization value)
      */
    override def compute(weightsOld: DenseVector[Double],
                         gradient: DenseVector[Double],
                         stepSize: Double,
                         iter: Int,
                         regParam: Double): (DenseVector[Double], Double) = {

        val thisIterStepSize = stepSize / math.sqrt(iter)
        val newWeights: DenseVector[Double] = weightsOld + gradient * (-thisIterStepSize)

        (newWeights, 0)
    }
}
