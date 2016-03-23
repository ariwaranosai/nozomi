package nozomi.nzmlib.dataprocess

import scala.io.Source

/**
  * Created by ariwaranosai on 16/3/22.
  *
  */

abstract class Reader{

    protected def lineSolver(s: String): Seq[String]

    def read(path: String, hasTitle: Boolean, skip: Int): Seq[Seq[String]] = {
        Source.fromFile(path).getLines().map(lineSolver).toSeq
    }

}
