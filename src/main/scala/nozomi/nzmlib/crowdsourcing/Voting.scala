package nozomi.nzmlib.crowdsourcing

import CSUtil._
import nozomi.util.NZMLogging
import breeze.linalg._
import CSUtil.labelData2Matrix

/**
  * Created by sai on 16-3-7.
  */

class Voting extends GeneralizedCSAlgorithm[OrdinaryCSModel]
    with NZMLogging { self =>

    override var optimizer: (Seq[LabeledData]) => (Seq[Double], Seq[Double]) = calcu


    override protected def createModel(solution: Seq[Double], workers: Seq[Double]): OrdinaryCSModel =
        new OrdinaryCSModel(solution, workers)


    private def calcuReliability(solution: DenseVector[Double])(x: ((Double, Int), (Int, Int))): (Int, Int) =  {
        if (x._1._1 == defaultLabel)
            x._2
        else if (x._1._1 != solution(x._1._2))
            (x._2._1, x._2._2 + 1)
        else
            (x._2._1 + 1, x._2._2 + 1)
    }


    private def calcu(data: Seq[LabeledData]): (Seq[Double], Seq[Double]) = {
        if (this.workerNum == -1)
            this.workerNum = data.map(_.person).max + 1

        if (this.entityNum == -1)
            this.entityNum = data.map(_.entity).max + 1

        logger.info(s"worker $workerNum, entity $entityNum")

        val dataMatrix = labelData2Matrix(data, entityNum, workerNum, defaultLabel)

        val solution = dataMatrix(*, ::).map(collectWithMax[Double](_, defaultLabel))

        val reliability = dataMatrix(::, *).map(dv => {
            val dd = dv.toArray.zipWithIndex
            val x = dd.toList.map(x => {
                if (x._1 == defaultLabel)
                    (0, 0)
                else if (x._1 == solution(x._2))
                    (1, 1)
                else
                    (0, 1)
            }).reduce((x, y) => (x._1 + y._1, x._2 + y._2))

            x._1 / x._2.toDouble
        })

        (solution.toArray, reliability.inner.toArray)

    }
}

object Voting {
    def apply(): Voting = new Voting()
}
