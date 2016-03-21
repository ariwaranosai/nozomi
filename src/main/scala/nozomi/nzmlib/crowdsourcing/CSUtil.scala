package nozomi.nzmlib.crowdsourcing

import scala.collection.mutable
import breeze.linalg.{DenseVector, DenseMatrix}

import scala.reflect.ClassTag

/**
  * Created by ariwaranosai on 16/3/17.
  *
  */

object CSUtil {

    /**
      * change labeledData to Matrix
      *
      * @param data LabeledData in Seq
      * @return matrix contains label in (entiry_id, persion_id)
      */
    def labelData2Matrix(data: Seq[LabeledData], rows: Int, cols: Int, default: => Double): DenseMatrix[Double] = {
        val matrix = DenseMatrix.fill[Double](rows, cols)(default)

        data.foreach(x => {
            matrix.update(x.entity, x.person, x.label)
        })

        matrix
    }

    def collectWithMax[T](dv: DenseVector[T], default: => T): T = {
        val collect = mutable.Map[T, Int]()

        dv.foreach(k => {
            collect.update(k, collect.getOrElse(k, 0) + 1)
        })

        collect.filter(x => x._1 != default).maxBy(_._2)._1
    }


    def collectWithMax[T: ClassTag](dv: DenseVector[T], weight: DenseVector[Double], default: => T): T = {
        val collect = mutable.Map[T, Double]()

        dv.toArray.zipWithIndex.filter(_._1 != default).foreach( k => {
            collect.update(k._1, collect.getOrElse(k._1, 0.0) + weight(k._2))
        })

        collect.maxBy(_._2)._1
    }


}
