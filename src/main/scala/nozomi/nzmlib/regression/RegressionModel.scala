package nozomi.nzmlib.regression

import breeze.linalg.Matrix

/**
  * Created by ariwaranosai on 16/2/29.
  *
  */

trait RegressionModel {

    /**
      * Predict values for given data set
      * @param testData Seq of data point
      * @return Seq of result
      */
    def predict(testData: Seq[Matrix[Double]]): Seq[Double]

    /**
      * Predict for a single data point
      * @param testData
      * @return
      */
    def predict(testData: Matrix[Double]): Double

}
