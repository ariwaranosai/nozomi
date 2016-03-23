package nozomi.nzmlib.dataprocess

/**
  * Created by ariwaranosai on 16/3/23.
  *
  */

abstract class Solver {
    protected def lineSolver(s: String): Seq[String]
}

trait CSVSolver extends Solver {
     def lineSolver(s: String): Seq[String] = {
        s.stripLineEnd.split(",")
    }
}
