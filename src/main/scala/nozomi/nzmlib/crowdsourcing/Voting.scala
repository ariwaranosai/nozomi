package nozomi.nzmlib.crowdsourcing

import breeze.linalg.DenseMatrix
import CSUtil._
import nozomi.util.NZMLogging
import breeze.linalg._
import CSUtil.labelData2Matrix

/**
  * Created by sai on 16-3-7.
  */

class Voting extends GeneralizedCSAlgorithm[OrdinaryCSModel]
    with NZMLogging {

    // TODO complete voting

    override var optimizer: (Seq[LabeledData]) => (Seq[Double], Seq[Double]) = ???


    override protected def createModel(solution: Seq[Double], workers: Seq[Double]): OrdinaryCSModel = ???

    private def calcu(data: Seq[LabeledData]): (Seq[Double], Seq[Double]) = {
        if (this.workerNum == -1)
            this.workerNum = data.map(_.person).max

        if (this.entityNum == -1)
            this.entityNum = data.map(_.entity).max

        logger.info(s"worker $workerNum, entity $entityNum")

        val dataMatrix = labelData2Matrix(data, entityNum, workerNum, defaultLabel)

        dataMatrix(::, *).map(collectWithMax[Double](_, defaultLabel))

        // TODO complete calcu
        (List(1.0), List(2.0))

    }
}
