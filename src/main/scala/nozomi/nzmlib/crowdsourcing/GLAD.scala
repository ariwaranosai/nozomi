package nozomi.nzmlib.crowdsourcing

import nozomi.util.NZMLogging

/**
  * Created by ariwaranosai on 16/3/28.
  *
  */

class GLAD extends GeneralizedCSAlgorithm[OrdinaryCSModel]
    with NZMLogging {

    var epsilon: Double = 0.5

    override var optimizer: Seq[LabeledData] => (Seq[Double], Seq[Double]) = _

    override protected def createModel(solution: Seq[Double], workers: Seq[Double]): OrdinaryCSModel = ???

}
