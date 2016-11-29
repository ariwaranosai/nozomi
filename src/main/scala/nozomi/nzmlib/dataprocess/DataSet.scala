package nozomi.nzmlib.dataprocess

import java.io.{File, PrintWriter}

import nozomi.nzmlib.dataprocess.SchemeT._

import scala.io.Source

/**
  * Created by ariwaranosai on 16/3/28.
  *
  */


class DataSet[T](val title: List[String],
                 val data: Seq[T],
                 val map: Map[Int, Map[String, Int]],
                 val template: Vector[Scheme]) {

    override def toString: String = {
        data.take(10).map(_.toString + "\n").mkString
    }

    def saveMap(path: String, col: Int): Unit = {
      val writer = new PrintWriter(new File(path))
      map(col) foreach {
          case (name, id) => writer.write(s"$name\t$id\n")
      }
      writer.close()
    }

  def saveAllMap(path: String): Unit = {
    map.keys foreach {
      col => saveMap(s"$path-$col", col)
    }
  }


  @inline
  private def toLineString(term: T, toString: T => Array[Any]): String = {
    val tarray = toString(term)
    assert(tarray.length == template.length)

    tarray.zip(template).zipWithIndex.foldRight("") {
      case (s, l) => scheme2String(s._1._1, s._1._2, map(s._2))
    }
  }

  def saveData(path: String, toString: T => String): Unit = {
    val writer = new PrintWriter(new File(path))

    data foreach { x =>
      writer.write(toString(x) + "\n")
    }

    writer.close()
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
