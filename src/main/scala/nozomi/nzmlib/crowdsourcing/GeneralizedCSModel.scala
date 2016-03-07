package nozomi.nzmlib.crowdsourcing

import nozomi.nzmlib.mlutil.{Loader, Saveable}

/**
  * Created by sai on 16-3-7.
  */

/**
  *  Generalized CrowdSourcing Model
  *  Generalized CrodSourcing Model is abstract Model representing model using
  *  crodsourcing model, including correct solution for problems and correct rate
  *  for every worker.
  *
  * @param solution Answers for problem
  * @param workers correct rate for every worker
  */
class OrdinaryCSModel(solution: Seq[Double],
                      workers: Seq[Double]) {
}
