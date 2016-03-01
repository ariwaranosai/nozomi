package nozomi.nzmlib.mlutil

import breeze.linalg.DenseVector

/**
  * Created by ariwaranosai on 16/3/1.
  *
  */

object MLUtil {

    /**
      * return a new vector with `1.0` (bias) appended to the input vector
      */
    def appendBias(v: DenseVector[Double]): DenseVector[Double] = {
        DenseVector.vertcat(v, DenseVector(1.0))
    }

}
