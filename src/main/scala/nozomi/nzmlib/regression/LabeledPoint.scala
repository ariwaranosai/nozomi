package nozomi.nzmlib.regression

import breeze.linalg.Matrix

/**
  * Created by ariwaranosai on 16/2/29.
  *
  */

case class LabeledPoint(label: Double, features: Matrix[Double]) {
    override def toString: String = {
        s"($label, $features)"
    }
}
