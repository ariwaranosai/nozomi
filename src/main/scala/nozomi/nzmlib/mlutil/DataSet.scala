package nozomi.nzmlib.mlutil

import scala.util.Random

/**
  * Created by ariwaranosai on 16/3/3.
  *
  */

class ImplicitDataSet[T](val data: Seq[T]) {
    def simple(frac: Double, seed: Int): ImplicitDataSet[T] = {
        val rand = new Random(seed)
        new ImplicitDataSet[T](data.filter(_ => rand.nextDouble() < frac))
    }
}


object DataSet {
    implicit def DataSetOps[T](data: Seq[T]): ImplicitDataSet[T] = new ImplicitDataSet[T](data)
}
