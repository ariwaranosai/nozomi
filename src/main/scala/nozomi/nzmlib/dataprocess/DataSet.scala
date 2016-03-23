package nozomi.nzmlib.dataprocess

/**
  * Created by ariwaranosai on 16/3/23.
  *
  */

class DataSet[T](data: Seq[T]) {
    lazy val size: Int = data.length

    def head(n: Int): Seq[T] = data.take(n)
}
