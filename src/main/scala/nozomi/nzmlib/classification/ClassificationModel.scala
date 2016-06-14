package nozomi.nzmlib.classification

import breeze.linalg.{DenseVector, Vector}

import scala.collection.GenSeq

/**
  * Created by ariwaranosai on 16/6/14.
  *
  */

trait ClassificationModel {
    type Label
    /**
      * Predict label for given data point
      * @param point single data point
      * @return label
      */
    def predict(point: Vector[Double]): Label

    /**
      * Predict for given data set
      * @param data GenSeq of data point
      * @return Seq of label
      */
    def predict(data: GenSeq[DenseVector[Double]]): Seq[Label] = {
        data map {predict(_)}
    }.toIndexedSeq
}
