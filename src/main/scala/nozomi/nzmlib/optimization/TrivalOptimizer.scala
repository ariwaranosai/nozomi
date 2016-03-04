package nozomi.nzmlib.optimization

import breeze.linalg.DenseVector

/**
  * Created by ariwaranosai on 16/3/4.
  *
  */

class TrivalOptimizer(
        solver: (Seq[(Double, DenseVector[Double])], DenseVector[Double]) => DenseVector[Double]
    ) extends Optimizer {

    var params: DenseVector[Double] = DenseVector.zeros[Double](0)

    def setParams(p: DenseVector[Double]) = {
        params = p.copy
    }

    override def optimize(data: Seq[(Double, DenseVector[Double])], initialWeights: DenseVector[Double]): DenseVector[Double] = {
        solver(data, params)
    }
}


object TrivalOptimizer {
    def apply(f: (Seq[(Double, DenseVector[Double])], DenseVector[Double]) => DenseVector[Double]): TrivalOptimizer =
        new TrivalOptimizer(f)
}
