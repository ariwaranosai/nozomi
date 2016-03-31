package nozomi.nzmlib.dataprocess

/**
  * Created by ariwaranosai on 16/3/23.
  *
  */

abstract class Solver {
    protected def lineSolver(s: String): List[String]
}

class CSVSolver extends Solver {
     def lineSolver(s: String): List[String] = {
        s.stripLineEnd.split(",").toList
    }
}

class SpaceSplitSolver(seq: String) extends Solver {
    def lineSolver(s: String): List[String] = {
        s.stripLineEnd.split(seq).toList
    }
}

object CSVSolver extends CSVSolver with Reader
object SpaceSplitSolver extends SpaceSplitSolver(" ") with Reader
object TabSplitSolver extends SpaceSplitSolver("\t") with Reader
