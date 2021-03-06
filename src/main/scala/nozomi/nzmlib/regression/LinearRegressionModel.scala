package nozomi.nzmlib.regression

import breeze.linalg.{inv, Matrix, DenseMatrix, DenseVector}
import breeze.stats.regression.leastSquares
import nozomi.nzmlib.mlutil.{Loader, Saveable}
import nozomi.nzmlib.optimization._
import nozomi.nzmlib.regression.impl.GLMRegressionModel
import nozomi.nzmlib.mlutil.MLUtil._

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

    override def save(path: String): Unit = {
        GLMRegressionModel.SaveLoad.save(path, this.getClass.getName, weights, intercept)
    }

    override protected def formatVersion: String = "1.0"

}


object LinearRegressionModel extends Loader[LinearRegressionModel] {
    override def load(path: String): LinearRegressionModel = {
        load(path)
    }
}

class LinearRegressionWithLSQ private[nzmlib]
    extends GeneralizedLinearAlgorithm[LinearRegressionModel] {
    /**
      * The optimizer to solve the problem
      *
      */
    override def optimizer: Optimizer = TrivalOptimizer(solver)

    def solver(data: Seq[(Double, DenseVector[Double])], params: DenseVector[Double]) = {
        val X: DenseMatrix[Double] = dataToMatrix(data.map(_._2))
        val y: DenseVector[Double] = DenseVector(data.map(_._1).toArray)
        println(X)
        println(y)

        val t = leastSquares(X.t, y)

        t.coefficients
    }


    override protected def createModel(weights: DenseVector[Double], intercept: Double): LinearRegressionModel =
        new LinearRegressionModel(weights, intercept)
}

object LinearRegressionWithLSQ {

    def train(input: Seq[LabeledPoint]): LinearRegressionModel = {
        new LinearRegressionWithLSQ().run(input)
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

    override val optimizer = new GradientDescent(gradient, updater)
        .setStepSize(stepSize)
        .setMiniBatchFraction(miniBatchFraction)
        .setNumIterations(numIterations)

    def this() = this(1.0, 100, 1.0)

    override protected[nzmlib] def createModel(weights: DenseVector[Double], intercept: Double) =
        new LinearRegressionModel(weights, intercept)

}


object LinearRegressionWithSGD {

    /**
      * Train a Linear regession model with given parameters.
 *
      * @param input dataset
      * @param numIterations  num of iterations
      * @param stepSize size of each step with SGD
      * @param miniBatchFraction sample fraction of data
      * @param initialWeights initial weights
      * @return
      */
    def train(
             input: Seq[LabeledPoint],
             numIterations: Int,
             stepSize: Double,
             miniBatchFraction: Double,
             initialWeights: DenseVector[Double]) = {
        new LinearRegressionWithSGD(stepSize, numIterations, miniBatchFraction)
            .run(input, initialWeights)
    }

    def train(
             input: Seq[LabeledPoint],
             numIterations: Int,
             stepSize: Double,
             miniBatchFraction: Double): LinearRegressionModel = {
        new LinearRegressionWithSGD(stepSize, numIterations, miniBatchFraction).run(input)
    }

    def train(
             input: Seq[LabeledPoint],
             numIterations: Int,
             stepSize: Double
             ): LinearRegressionModel = {
        train(input, numIterations, stepSize, 1.0)
    }

    def train(
             input: Seq[LabeledPoint],
             numIterations: Int): LinearRegressionModel = {
        train(input, numIterations, 1.0, 1.0)
    }
}
