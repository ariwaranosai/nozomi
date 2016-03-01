package nozomi.nzmlib.regression

import breeze.linalg.{DenseVector}

/**
  * Created by ariwaranosai on 16/2/29.
  *
  */

trait RegressionModel {

    /**
      * Predict values for given data set
 *
      * @param testData Seq of data point
      * @return Seq of result
      */
    def predict(testData: Seq[DenseVector[Double]]): Seq[Double]

    /**
      * Predict for a single data point
 *
      * @param testData for a single data point
      * @return
      */
    def predict(testData: DenseVector[Double]): Double

}
