package nozomi.nzmlib.crowdsourcing

import scala.language.implicitConversions

/**
  * Created by sai on 16-3-7.
  */


case class LabeledData(entity: Int, person: Int, label: Double) {
    override def toString = {
        s"Problem $entity is labeled as ${label.toString} by persion $person"
    }
}

object LabeledData {
    implicit def LabeledDataCos(s: Array[Any]): LabeledData = {
        LabeledData(s(0).asInstanceOf[Int], s(1).asInstanceOf[Int], s(2).asInstanceOf[Int])
    }
}
