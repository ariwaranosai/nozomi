package nozomi.nzmlib.crowdsourcing

import nozomi.nzmlib.dataprocess.{CSVSolver, DataSet, SchemeT}
import nozomi.nzmlib.dataprocess.SchemeT.{SEnum, SInt, Scheme}
import org.scalatest.FlatSpec

import scala.io.Source

/**
  * Created by ariwaranosai on 16/4/25.
  *
  */

class LabeledDataTest extends FlatSpec {

    "row resolver" must "be done right" in {
        val rawTruth =
            """A,1
              |B,1
              |C,1
              |D,1
              |E,1
            """.stripMargin

        val t = Vector[Scheme](SEnum, SEnum, SInt)


        val rawData: String =
            """0,A,0
              |0,C,0
              |0,D,1
              |0,E,1
              |1,A,0
              |1,B,4
              |1,C,1
              |1,E,0
              |2,B,1
              |2,C,1
              |2,E,0
              """.stripMargin

        def so(x: List[String]):List[List[String]] = {
            if (x(2).toInt > 1)
                List()
            else
                List(List(x(1), x.head, x(2)))
        }


        val data = DataSet[LabeledData](t)(
            CSVSolver,
            Source.fromString(rawData),
            rowSolver = so
        )

        val res = Voting().setDefaultLabel(-1).run(data)

        val s = DataSet[Truth](Vector[Scheme](SEnum, SInt), data.map)(
            CSVSolver,
            Source.fromString(rawTruth)
        )

        val l: Double = GroundTruth(s).calcuAcc(res.solution.toVector)
        assert(l == 3/5.0)
    }

}
