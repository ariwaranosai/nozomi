package nozomi.nzmlib.optimization

import breeze.linalg.Matrix

/**
  * Created by ariwaranosai on 16/2/29.
  *
  */

trait Optimizer {
    def optimize(data: Seq[(Double, Matrix[Double])], initialWeights: Matrix[Double]): Matrix[Double]
}
