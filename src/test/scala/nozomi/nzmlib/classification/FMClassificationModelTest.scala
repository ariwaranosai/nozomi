package nozomi.nzmlib.classification

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.FlatSpec

/**
  * Created by ariwaranosai on 16/6/14.
  *
  */

class FMClassificationModelTest extends FlatSpec {
    "FactorizationMachineModel" must "compute right" in {
        val w0 = 2.0
        val w: DenseVector[Double] = DenseVector(1.0, -2.0, 1)
        val v: DenseMatrix[Double] = DenseMatrix((1.0, -1.0), (-2.0, 1.0), (-1.0, 2.0))
        val x: DenseVector[Double] = DenseVector(-1.5, 1.0, 2.0)

        val m = new FMClassificationModel(w0, w, v)

        assert(m.evalFMValue(x) == 22.0)
        assert(m.predict(x) == 1)
    }
}
