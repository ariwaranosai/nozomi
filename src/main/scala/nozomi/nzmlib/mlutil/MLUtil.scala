package nozomi.nzmlib.mlutil

import breeze.linalg.{DenseMatrix, Transpose, Matrix, DenseVector}

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

    def dataToMatrix(data: Seq[DenseVector[Double]]): DenseMatrix[Double] = {
        val row = data.length
        val cols = data.head.length


        DenseMatrix.create(cols, row, data.toArray.flatMap(_.toArray))
    }

}
