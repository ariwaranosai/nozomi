package nozomi.nzmlib.crowdsourcing

/**
  * Created by ariwaranosai on 16/3/31.
  *
  */

class GroundTruth(truth: Seq[Truth]) {
    // TODO complete calcu Acc
}

object GroundTruth {

}

case class Truth(Entity: Int, Label: Int) {
    override def toString =
        s"${Entity.toString} is ${Label.toString}\t"
}

object Truth {
    implicit def TruthCos(s: Array[Any]): Truth = {
        new Truth(s(0).asInstanceOf[Int], s(1).asInstanceOf[Int])
    }
}

