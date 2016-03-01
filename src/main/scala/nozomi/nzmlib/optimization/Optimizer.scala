package nozomi.nzmlib.optimization

import breeze.linalg.DenseVector

/**
  * Created by ariwaranosai on 16/2/29.
  *
  */

trait Optimizer {
    def optimize(data: Seq[(Double, DenseVector[Double])], initialWeights: DenseVector[Double]): DenseVector[Double]
}
