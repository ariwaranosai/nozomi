package nozomi.nzmlib.regression

import breeze.linalg.DenseVector

/**
  * Created by ariwaranosai on 16/2/29.
  *
  */

case class LabeledPoint(label: Double, features: DenseVector[Double]) {
    override def toString: String = {
        s"($label, $features)"
    }
}
