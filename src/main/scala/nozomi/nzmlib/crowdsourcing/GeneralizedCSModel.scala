package nozomi.nzmlib.crowdsourcing

import java.io.PrintWriter

import nozomi.nzmlib.mlutil.{Loader, Saveable}
import org.json4s._
import org.json4s.JsonDSL.WithDouble._
import org.json4s.native.JsonMethods._

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

class GeneralizedCSModel(solution: Seq[Double],
                         workers: Seq[Double]) extends Saveable{

    lazy val workerNum = workers.size
    lazy val entityNum = solution.size

    /**
      * @param path path to save model
      */
    override def save(path: String): Unit =
        GeneralizedCSModel.Save.save(path, this.getClass.getName, workers, solution)

    override protected def formatVersion: String = "0.1"
}

object GeneralizedCSModel {
    object Save {

        def formatVersion: String = "0.1"

        def save[T](path: String,
                    modelClass: String,
                    workers: Seq[T],
                    solution: Seq[T]): Unit = {

            val modeldata = ("modelClass" -> modelClass) ~ ("formatVersion" -> formatVersion) ~
                ("data" -> (("works" -> workers.map(_.toString).toList) ~ ("solution" -> solution.map(_.toString).toList)))

            val modelString = compact(render(modeldata))

            Some(new PrintWriter(path)).foreach { p => p.write(modelString); p.close() }
        }

    }

}

class OrdinaryCSModel(solution: Seq[Double], workers: Seq[Double])
    extends GeneralizedCSModel(solution, workers) {

    def getReliabilityById(index: Int) = workers(index)

    def getSolutionById(index: Int) = solution(index)

}
