package nozomi.nzmlib.crowdsourcing

/**
  * Created by ariwaranosai on 16/3/18.
  *
  */

abstract class GeneralizedCSAlgorithm[M <: GeneralizedCSModel]{

    protected var workerNum = -1
    protected var entityNum = -1
    protected var defaultLabel: Double = 0

    def setWorkerNum(num: Int): this.type = {
        this.workerNum = num
        this
    }

    def setEntityNum(num: Int): this.type = {
        this.entityNum = num
        this
    }

    def setDefaultLabel(label: Double): this.type = {
        this.defaultLabel = label
        this
    }

    var optimizer: Seq[LabeledData] => (Seq[Double], Seq[Double])

    protected def createModel(solution: Seq[Double], workers:Seq[Double]): M

    def run(data: Seq[LabeledData]): M = {
        val (solution, workers) = optimizer(data)
        createModel(solution, workers)
    }

}
