package nozomi.nzmlib.dataprocess

import scala.io.Source
import SchemeT._

/**
  * Created by ariwaranosai on 16/3/22.
  *
  */

trait Reader{

    protected def lineSolver(s: String): List[String]

    def read(path: String, hasTitle: Boolean, skip: Int): (List[String], Seq[List[String]]) = {
        if (hasTitle) {
            val source = Source.fromFile(path)
            val title = lineSolver(source.take(0).mkString)

            (title, source.getLines().map(lineSolver).toSeq)
        } else {
            val source = Source.fromFile(path).getLines().map(lineSolver).toSeq
            (List[String](), source)
        }
    }
}


