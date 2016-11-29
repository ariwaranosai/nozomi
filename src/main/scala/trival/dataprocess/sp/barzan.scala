package trival.dataprocess.sp

import java.io.{File, PrintWriter}

import nozomi.nzmlib.crowdsourcing.LabeledData
import nozomi.nzmlib.crowdsourcing.LabeledData._
import nozomi.nzmlib.dataprocess.{DataSet, TabSplitSolver}
import nozomi.nzmlib.dataprocess.SchemeT.{SEnum, SInt, Scheme}

import scala.io.Source

/**
  * Created by ariwaranosai on 2016/11/29.
  *
  */

object barzan {
  def main(args: Array[String]): Unit = {
    val path = "/Users/shihang/code/scala/Nozomi/Data/barzanMozafari/"

    val scheme = Vector[Scheme](SEnum, SInt, SInt)

    val datafile = path + "labels.txt"

    val data = DataSet[LabeledData](scheme)(TabSplitSolver, Source.fromFile(datafile), hasTitle = false, 0)

    data.saveAllMap(path + "map")
    data.saveData(path + "data", labeledDateString)

    val matrix = labeledDatesMatrix((data.data, 83, 1000))

    val writer = new PrintWriter(new File(path + "matrix"))


    (0 until 83) foreach { x =>
      writer.write(matrix(x, ::).inner.data.mkString(",") + "\n")
    }

    writer.close()
  }
}
