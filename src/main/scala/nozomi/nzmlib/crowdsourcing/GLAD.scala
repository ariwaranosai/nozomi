package nozomi.nzmlib.crowdsourcing

import nozomi.util.NZMLogging

/**
  * Created by ariwaranosai on 16/3/28.
  *
  */

class GLAD extends GeneralizedCSAlgorithm[OrdinaryCSModel]
    with NZMLogging { self =>

    var epsilon: Double = 0.5
    var maxIterations: Int = 1000
    var workerNumber: Int = -1
    var entityNumber: Int = -1

    var alpha: Vector[Double] = Vector()
    var beta: Vector[Double] = Vector()

    // todo complete EM
    override var optimizer: Seq[LabeledData] => (Seq[Double], Seq[Double]) = _

    // todo createModel
    override protected def createModel(solution: Seq[Double], workers: Seq[Double]): OrdinaryCSModel = ???

    def setEpsilon(e: Double): this.type = {
        self.epsilon = e
        self
    }

    def setMaxIterations(m: Int): this.type = {
        self.maxIterations = m
        self
    }

    def setWorkerNumber(w: Int): this.type = {
        self.workerNumber = w
        self
    }

    def setEntityNumber(e: Int): this.type = {
        self.entityNumber = e
        self
    }
}

