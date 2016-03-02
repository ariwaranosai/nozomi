package nozomi.nzmlib.optimization

import breeze.linalg.DenseVector

/**
  * Created by ariwaranosai on 16/3/2.
  *
  */

abstract class Gradient {

    /**
      * Compute the gradient and loss for a single point
      * @param data features of a single point
      * @param label label of this data point
      * @param weights weights/coefficients corresponding to features
      * @return
      */
    def compute(data: DenseVector[Double], label: Double, weights: DenseVector[Double]): (DenseVector[Double], Double) = {
        val gradient = DenseVector.zeros[Double](weights.length)

        val loss: Double = compute(data, label, weights, gradient)
        (gradient, loss)
    }

    /**
      * Compute the gradient and loss given the features of a single data point,
      * add the gradient to a provided vector to avoid creating new objects, return loss.
      * @param data a single data point
      * @param label label for this data point
      * @param weights weights/coefficients corresponding to features
      * @param cumGradient the computed gradient will be added to this vector
      * @return loss
      */
    def compute(data: DenseVector[Double], label: Double, weights: DenseVector[Double], cumGradient: DenseVector[Double]): Double
}

class LeastSquaresGradient extends Gradient {
    override def compute(data: DenseVector[Double], label: Double, weights: DenseVector[Double]): (DenseVector[Double], Double) = {
        val diff: Double = weights.dot(data) - label
        val loss = diff * diff / 2.0
        val gradient = data * diff

        (gradient, loss)
    }

    override def compute(data: DenseVector[Double], label: Double, weights: DenseVector[Double], cumGradient: DenseVector[Double]): Double = {
        val diff: Double = weights.dot(data) - label
        cumGradient += data * diff
        diff * diff / 2.0
    }
}