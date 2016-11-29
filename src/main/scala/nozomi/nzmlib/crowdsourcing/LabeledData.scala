package nozomi.nzmlib.crowdsourcing

import breeze.linalg.{CSCMatrix, DenseMatrix, Matrix}

import scala.language.implicitConversions

/**
  * Created by sai on 16-3-7.
  */


case class LabeledData(entity: Int, person: Int, label: Double) {
    override def toString = {
        s"Problem $entity is labeled as ${label.toString} by person $person"
    }
}


object LabeledData {
    implicit def labeledDataCos(s: Array[Any]): LabeledData = {
        LabeledData(s(1).asInstanceOf[Int], s(0).asInstanceOf[Int], s(2).asInstanceOf[Int])
    }

    implicit def labeledDateString(l: LabeledData): String =
        s"${l.entity}\t${l.person}\t${l.label}"

    def labeledDatesMatrix(l: (Seq[LabeledData], Int, Int)): DenseMatrix[Int] = {
      val matrix = DenseMatrix.fill[Int](l._2, l._3)(-1)
      l._1 foreach { x =>
        matrix.update(x.person, x.entity - 1, x.label.toInt)
      }
      matrix
    }
}
