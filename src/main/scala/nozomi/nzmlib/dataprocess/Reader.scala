package nozomi.nzmlib.dataprocess

import scala.io.Source
import SchemeT._

/**
  * Created by ariwaranosai on 16/3/22.
  *
  */

trait Reader{

    protected def lineSolver(s: String): List[String]


    //todo buildfrom source

    /**
      * read
      * @param source Source
      * @param hasTitle hasTitle or not
      * @param skip skip head line
      * @return (title cols, raw data)
      */
    def read(source: Source, hasTitle: Boolean, skip: Int): (List[String], Seq[List[String]]) = {
        if (hasTitle) {
            val title = lineSolver(source.take(0).mkString)

            (title, source.getLines().map(lineSolver).filter(_.nonEmpty).toSeq)
        } else {
            val rdata = source.getLines().map(lineSolver).filter(_.nonEmpty).toSeq
            (List[String](), rdata)
        }
    }
}

