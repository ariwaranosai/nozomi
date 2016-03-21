package nozomi.nzmlib.crowdsourcing

import breeze.linalg.DenseVector
import org.scalatest.FlatSpec

/**
  * Created by ariwaranosai on 16/3/21.
  *
  */

class CSUtilTest extends FlatSpec {

    "collectWithMax" must "get most times key" in {
        val vd = DenseVector(1, 2, 2, 3, 3, 5, 5, 2)

        val max = CSUtil.collectWithMax(vd, -1)

        assert(max == 2)
    }

    "collectWithMax" should "ignore default" in {
        val vd = DenseVector(1, 2, 2, 3, 3, 2, 2, 5, 5, 5, 2)

        val max = CSUtil.collectWithMax(vd, 2)

        assert(max == 5)
    }


}
