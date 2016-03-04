package nozomi.nzmlib.regression

import breeze.linalg.{norm, DenseVector}
import org.scalatest.FlatSpec

/**
  * Created by ariwaranosai on 16/3/4.
  *
  */

class LinearRegressionWithLSQTest extends FlatSpec {

    "Linear Regression with Least Square" should "right" in {
        val x1 = DenseVector[Double](1, 1)
        val x2 = DenseVector[Double](2, -2)
        val x3 = DenseVector[Double](3, 3)
        val x4 = DenseVector[Double](4, 5)
        val x5 = DenseVector[Double](9, -3)
        val x6 = DenseVector[Double](9, -2)

        val data = List((5.0, x1), (3.0, x2), (9.0, x3), (12.0, x4),
            (9.0, x5), (10.0, x6))
            .map(x => LabeledPoint(x._1, x._2))

        val m = new LinearRegressionWithLSQ().run(data)
        assert(norm(m.weights - DenseVector[Double](1,1)) < 1e-3 &&
            math.abs(m.intercept - 3.0) < 1e-3)
    }

}
