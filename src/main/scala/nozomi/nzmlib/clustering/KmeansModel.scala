package nozomi.nzmlib.clustering

import breeze.linalg.DenseVector
import nozomi.nzmlib.mlutil.Saveable

/**
  * Created by sai on 16-3-7.
  */
class KmeansModel(val clusterCenters: Seq[DenseVector[Double]]) extends Saveable { self =>

    def k: Int = clusterCenters.length

    def predict(point: DenseVector[Double]): Int = ???

    def predict(points: Seq[DenseVector[Double]]): Seq[Int] =
        points.map(x => predict(x))

    def computeCost(data: Seq[DenseVector[Double]]) = ???

    /**
      * @param path path to save model
      */
    override def save(path: String): Unit = ???

    override protected def formatVersion: String = ???
}
