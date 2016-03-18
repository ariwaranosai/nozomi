package nozomi.nzmlib.crowdsourcing

import nozomi.util.NZMLogging

/**
  * Created by sai on 16-3-7.
  */

class Voting extends GeneralizedCSAlgorithm[OrdinaryCSModel]
    with NZMLogging {

    // TODO complete voting

    override var optimizer: (Seq[LabeledData]) => (Seq[Double], Seq[Double]) = _

    override protected def createModel(solution: Seq[Double], workers: Seq[Double]): OrdinaryCSModel = ???
}
