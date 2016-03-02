package nozomi.nzmlib.regression

import breeze.linalg.DenseVector
import nozomi.nzmlib.mlutil.{Loader, Saveable}
import nozomi.nzmlib.optimization.{SimpleUpdater, LeastSquaresGradient}

/**
  * Created by ariwaranosai on 16/3/2.
  *
  */

class LinearRegressionModel(
        override val weights: DenseVector[Double],
        override val intercept: Double) extends GeneralizedLinearModel (weights, intercept)
    with RegressionModel with Saveable {
    /**
      * Predict values for this given data point
      *
      * @param data      Row vector containing the feature for this data point
      * @param weights   Column vector containing the weights of the model
      * @param intercept Intercept of the model
      * @return
      */
    override protected def predictPoint(data: DenseVector[Double], weights: DenseVector[Double], intercept: Double): Double = {
        weights.dot(data) + intercept
    }

}


object LinearRegressionModel extends Loader[LinearRegressionModel] {
    override def load(path: String): LinearRegressionModel = {
        load(path)
    }
}

/**
  * Train a linear regression model with no regularization using Stochastic Gradient Descent
  * Objective function
  *             arg min_{w,b} 1/n ||Y - W'X||_2
  */

class LinearRegressionWithSGD private[nzmlib](
        private var stepSize: Double,
        private var numIterations: Int,
        private var miniBatchFraction: Double)
    extends GeneralizedLinearAlgorithm[LinearRegressionModel] {

    private val gradient = new LeastSquaresGradient()
    private val updater = new SimpleUpdater()

    // override val optimizer = new GradientDescent(gradient, updater)

}
