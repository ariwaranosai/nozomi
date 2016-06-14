package nozomi.nzmlib.classification

import breeze.linalg.{Matrix, Vector}
import nozomi.nzmlib.mlutil.Saveable
import breeze.numerics.sigmoid

/**
  * Created by ariwaranosai on 16/6/14.
  *
  */

/**
  * Naive Factorization Machines with libFM
  * @param w0 wight_0
  * @param w wights vector p * 1
  * @param v matrix p * k
  */
class FMClassificationModel(w0:Double,
                           w: Vector[Double],
                           v: Matrix[Double]
                           ) extends ClassificationModel with Saveable {

    lazy val k: Int = v.cols
    lazy val p: Int = v.rows

    override type Label = Int

    private def evalFMValue(point: Vector[Double]): Double = {
        assert(point.size == w.size, "data and weight dim should the same")
        // compute \sum_{j = 1}^p w_j x_j + w_0
        val order1 =  (w.t * point) + w0
        // compute others
        // follow paper
        val order2 = (0 to k).par.map(f => {
            val square_x = math.pow((0 to p).par.map(j => {
                v(j, f) * point(j)
            }).sum, 2)
            val square_sum = (0 to p).par.map( j => math.pow(v(j, f), 2) * math.pow(point(j), 2)).sum

            square_x - square_sum
        }).sum * 0.5

        order1 + order2
    }

    /**
      * Predict label for given data point
      *
      * @param point single data point
      * @return label
      */
    override def predict(point: Vector[Double]): Label = {
        val value = evalFMValue(point)
        val p = sigmoid(value)
        if (p > 0.5) 1 else 0
    }

    override protected def formatVersion: String = "0.01"

    /**
      * @param path path to save model
      */
    override def save(path: String): Unit = ???
}
