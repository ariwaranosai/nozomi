package nozomi.nzmlib.mlutil

import scala.util.Random

/**
  * Created by ariwaranosai on 16/3/3.
  *
  */

class DataSet[T](val data: Seq[T]) {
    def simple(frac: Double, seed: Int): DataSet[T] = {
        val rand = new Random(seed)
        new DataSet[T](data.filter(_ => rand.nextDouble() < frac))
    }
}


object DataSet {
    implicit def DataSetOps[T](data: Seq[T]): DataSet[T] = new DataSet[T](data)
}
