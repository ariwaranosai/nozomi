package nozomi.nzmlib.mlutil

import breeze.linalg.{DenseMatrix, Matrix}

/**
  * Created by ariwaranosai on 16/3/1.
  *
  */

object MLUtil {

    /**
      * return a new vector with `1.0` (bias) appended to the input vector
      */
    def appendBias(v: Matrix[Double]): Matrix[Double] = {
        val rows = v.rows
        DenseMatrix.horzcat(v.toDenseMatrix, DenseMatrix.ones[Double](rows,1))
    }

}
