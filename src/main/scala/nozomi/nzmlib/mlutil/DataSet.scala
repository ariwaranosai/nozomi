package nozomi.nzmlib.mlutil

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by ariwaranosai on 16/3/3.
  *
  */

class ImplicitDataSet[T](val data: Seq[T]) {
    def simple(frac: Double, seed: Long): Seq[T] = {
        val rand = new Random(seed)
        data.filter(_ => rand.nextDouble() < frac)
    }

  /**
    * fixed size sampling from long array
    * @param num size
    * @param seed seed for random
    * @return
    */
    def simple(num: Int, seed: Long): Seq[T] = {
        val rand = new Random(seed)
      if (num == 0) {
        List[T]()
      } else {
        val count = data.size
        if (count <= num)
          data
        else {
          val sq = new ArrayBuffer[T]()
          data.zipWithIndex.foreach {
            case (x, y) if y < num => sq += x
            case (x, y) => if (rand.nextDouble() < num.toDouble / y) {
              val index = rand.nextInt(num)
              sq(index) = x
            }
          }
          sq
        }
      }

    }

}


object DataSet {
    implicit def DataSetOps[T](data: Seq[T]): ImplicitDataSet[T] = new ImplicitDataSet[T](data)
}
