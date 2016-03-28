package nozomi.nzmlib.dataprocess

import nozomi.nzmlib.dataprocess.SchemeT.Scheme

/**
  * Created by ariwaranosai on 16/3/28.
  *
  */


class DataSet[T](val title: List[String],
                 val data: Seq[T],
                 val map: Map[Int, Map[String, Int]],
                 val template: Vector[Scheme]) {

    override def toString = {
        data.take(10).map(_.toString).mkString
    }

    // TODO add saveable

}

object DataSet {
    def apply[T](template: Vector[Scheme])
                (reader: Reader, path: String, hasTitle: Boolean, skip: Int)
                (implicit cos: Array[Any] => T): DataSet[T] = {
        val (title, rdata) = reader.read(path, hasTitle, skip)
        val solver = DataResolver(rdata).setTemplate(template)
        val data = solver.transform[T]
        new DataSet[T](title, data, solver.maps, template)
    }
}
