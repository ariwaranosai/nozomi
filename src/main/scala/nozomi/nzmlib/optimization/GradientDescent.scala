package nozomi.nzmlib.optimization

import breeze.linalg.DenseVector
import nozomi.util.NZMLogging
import scala.collection.mutable.ArrayBuffer

/**
  * Created by ariwaranosai on 16/3/2.
  *
  */

class GradientDescent private[nzmlib] (private var gradient: Gradient,
    private var updater: Updater) extends Optimizer with NZMLogging {

    private var stepSize: Double = 1.0
    private var numIterations: Int = 100
    private var regParam: Double = 0.0
    private var miniBatchFraction: Double = 1.0
    private var convergenceToL: Double = 0.001


    def setStepSize(step: Double): this.type = {
        this.stepSize = step
        this
    }

    def setMiniBatchFraction(fraction: Double): this.type = {
        this.miniBatchFraction = fraction
    }

    def setNumIterations(iters: Int): this.type = {
        this.numIterations = iters
        this
    }

    def setRegParam(regParam: Double): this.type = {
        this.regParam = regParam
        this
    }

    def setConvergenceToL(tolerance: Double): this.type = {
        require(tolerance <= 1.0 && 0 <= tolerance)
        this.convergenceToL = tolerance
        this
    }

    def setGradient(gradient: Gradient): this.type = {
        this.gradient = gradient
        this
    }

    def setUpdater(updater: Updater): this.type = {
        this.updater = updater
        this
    }


    def optimize(data: Seq[(Double, DenseVector[Double])], initialWeights: DenseVector[Double]): DenseVector[Double] = {
        val (weights, _) = GradientDescent.runMiniBatchSGD(data, gradient,
            updater, stepSize, numIterations, regParam, miniBatchFraction, initialWeights, convergenceToL)
    }
}


object GradientDescent extends NZMLogging {

    /**
      * Run Stochastic gradient descent.
      *
      * @param data Input data for SGD
      * @param gradient Gradient object (used to compute the gradient of the loss function of
      *                 one single data example)
      * @param updater Updater function to actully perform a gradient step in a given direction
      * @param stepSize inital step size for the first step
      * @param numIterations number of Iterations to run
      * @param regParam regularizaiton parameter
      * @param miniBatchFraction fraction of the input data should be used for one iteration of SGD.
      * @param initialWeights  initial weights
      * @param convergenceToL Minibatch iteration will end before numIterations if the relative
      *                       difference between the current weight and the previous weight.
      *                       In measuring convergence, L2 norm is calculated.
      * @return (final weights, stochastic loss computed for every iteration)
      */
    def runMiniBatchSGD(
            data: Seq[(Double, DenseVector[Double])],
            gradient: Gradient,
            updater: Updater,
            stepSize: Double,
            numIterations: Int,
            regParam: Double,
            miniBatchFraction: Double,
            initialWeights: DenseVector[Double],
            convergenceToL: Double): (DenseVector[Double], Array[Double]) = {

        if (miniBatchFraction < 1.0 && convergenceToL > 0.0) {
            logger.warn("Testing against a convergenceTol when using miniBatchFraction " +
                "< 1.0 can be unstable because of the stochasticity in sampling")
        }

        val stochasticLossHistory = new ArrayBuffer[Double](numIterations)

        var previousWeights: Option[DenseVector[Double]] = None
        var currentWeights: Option[DenseVector[Double]] = None

        val numExamples = data.length

        if (numExamples == 0) {
            logger.warn("GradientDescent.runMiniBatchSGD returning initial weights, no data found")
            return (initialWeights, stochasticLossHistory.toArray)
        }

        if (numExamples * miniBatchFraction < 1) {
            logger.warn("The miniBatchFraction is too small")
        }

        var weights = initialWeights
        val n = weights.length

        /**
          * For the first iteration, the regVal will be initialized as sum of weight squares
          * if it's L2 updater, it is the same for L1
          */

        var regVal = updater.compute(weights,
            DenseVector.zeros[Double](weights.length), 0, 1, regParam)._2

        var converged = false
        var i = 1

        while (!converged && i <= numIterations) {
            // TODO 把采样注入到Seq里面
        }
    }
}
