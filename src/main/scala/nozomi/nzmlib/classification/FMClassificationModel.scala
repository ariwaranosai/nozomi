package nozomi.nzmlib.classification

import breeze.linalg.{Matrix, Vector}
import nozomi.nzmlib.mlutil.Saveable
import breeze.numerics.sigmoid
import nozomi.nzmlib.optimization.FactorizationMachineModel

/**
  * Created by ariwaranosai on 16/6/14.
  *
  */

/**
  * Naive Factorization Machines
  * @param w0 wight_0
  * @param w wights vector p * 1
  * @param v matrix p * k
  */
class FMClassificationModel(w0:Double,
                           w: Vector[Double],
                           v: Matrix[Double]
                           ) extends FactorizationMachineModel(w0, w, v)
    with ClassificationModel with Saveable {

    override type Label = Int

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

    // todo
    /**
      * @param path path to save model
      */
    override def save(path: String): Unit = ???
}
