package nozomi.nzmlib.dataprocess

import nozomi.nzmlib.dataprocess.SchemeT.Scheme

import scala.io.Source

/**
  * Created by ariwaranosai on 16/3/28.
  *
  */


class DataSet[T](val title: List[String],
                 val data: Seq[T],
                 val map: Map[Int, Map[String, Int]],
                 val template: Vector[Scheme]) {

    override def toString = {
        data.take(10).map(_.toString + "\n").mkString
    }

    // TODO add saveable

}

object DataSet {

    def apply[T](template: Vector[Scheme], maps: Map[Int, Map[String, Int]] = null)
                (reader: Reader,
                 source: Source,
                 hasTitle: Boolean = false,
                 skip: Int = 0,
                 rowSolver: List[String] => List[List[String]] = List(_))
                (implicit cos: Array[Any] => T): DataSet[T] = {
        val (title, rdata) = reader.read(source, hasTitle, skip)
        val sdata = rdata.flatMap(rowSolver)

        val solver = if (maps == null)  DataResolver(sdata).setTemplate(template)
                    else DataResolver(sdata).setTemplate(template).setMaps(maps)

        val data = solver.transform[T]
        new DataSet[T](title, data, solver.maps, template)
    }

    implicit def string2source(s: String): Source = Source.fromString(s)

}
