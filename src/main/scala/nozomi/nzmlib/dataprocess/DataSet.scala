package nozomi.nzmlib.dataprocess

import SchemeT._

import scala.reflect._


/**
  * Created by ariwaranosai on 16/3/23.
  *
  */

abstract class DataResolver(data: Seq[List[String]]) {
    protected var maps = Map[Int, Map[String, Int]]()
    protected var template = Vector[Scheme]()

    // TODO collect cols
    def collectCols(s: Int) = data.map(entry => {
        entry(s)
    }).toSet.zipWithIndex.toMap

    def collectCols(l: List[Int]) = l.map((index: Int) => (index, {
        data.map((entry: List[String]) => entry(index))
    }.toSet[String].zipWithIndex.toMap)).toMap


    // TODO add comment
    def transform[T](template: Vector[Scheme], f: Array[Any] => T): Seq[T] = {
        val hasEnum = template.zipWithIndex.filter(_._1 == SEnum)

        if (hasEnum.nonEmpty) {
            maps = collectCols(hasEnum.map(_._2).toList)
        }

        data.map(row => {
            val params = row.zip(template).zipWithIndex.map({
                case ((str, SInt), i) => str.toInt
                case ((str, SDouble), i) => str.toDouble
                case ((str, SString), i) => str.toString
                case ((str, SEnum), i) => maps(i)(str)
            })

            f(params.toArray)
        })
    }
}

object SchemeT {
    abstract class Scheme
    case object SInt extends Scheme
    case object SDouble extends Scheme
    case object SEnum extends Scheme
    case object SString extends Scheme
}
