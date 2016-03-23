package nozomi.nzmlib.crowdsourcing

import org.scalatest.FlatSpec

/**
  * Created by ariwaranosai on 16/3/21.
  *
  */

class VotingTest extends FlatSpec {
    "Voting" must "get correct solution" in {
        val data = List[LabeledData](
            LabeledData(0, 1, 1),
            LabeledData(0, 2, 0),
            LabeledData(0, 0, 0),
            LabeledData(1, 1, 1),
            LabeledData(1, 0, 1),
            LabeledData(2, 2, 0)
        )

        val crowd = new Voting().setDefaultLabel(-1).run(data)

        assert(crowd.getSolutionById(0) == 0)
        assert(crowd.getSolutionById(1) == 1)
        assert(crowd.getSolutionById(2) == 0)

        assert(crowd.getReliabilityById(0) == 1)
        assert(crowd.getReliabilityById(1) == 0.5)
        assert(crowd.getReliabilityById(2) == 1)
    }

}
