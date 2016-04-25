package nozomi.nzmlib.crowdsourcing

import nozomi.nzmlib.dataprocess.DataSet

/**
  * Created by ariwaranosai on 16/3/31.
  *
  */

class GroundTruth(truth: Seq[Truth]) {
    def calcuAcc[T](vector: Vector[T]): Double = {
        val rights = truth.foldRight(0)((row, count) => {
            if (vector(row.Entity) == row.Label)
                count + 1
            else
                count
        })
        rights / truth.length.toDouble
    }

    def calcuAcc[T](vector: Array[T]): Double = {
        calcuAcc(vector.toVector)
    }
}

object GroundTruth {
    def apply(truth: DataSet[Truth]): GroundTruth = new GroundTruth(truth.data)
}

case class Truth(Entity: Int, Label: Int) {
    override def toString =
        s"${Entity.toString} is ${Label.toString}\n"
}

object Truth {
    implicit def TruthCos(s: Array[Any]): Truth = {
        new Truth(s(0).asInstanceOf[Int], s(1).asInstanceOf[Int])
    }
}
