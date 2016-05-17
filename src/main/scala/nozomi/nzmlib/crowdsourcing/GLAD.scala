package nozomi.nzmlib.crowdsourcing

import nozomi.util.NZMLogging
import breeze.stats.distributions.{Uniform, Bernoulli}
import math._

/**
  * Created by ariwaranosai on 16/3/28.
  *
  */

class GLAD extends GeneralizedCSAlgorithm[OrdinaryCSModel]
    with NZMLogging { self =>

    var epsilon: Double = 0.5
    var maxIterations: Int = 1000
    var workerNumber: Int = -1
    var entityNumber: Int = -1


    // labeler's ability
    var alpha: Option[Array[Double]] = None
    // object's difficulty
    var beta: Option[Array[Double]] = None
    // label for objects
    var z_truth: Option[Array[Int]] = None

    // prior
    var priorZ_1: Option[Array[Double]] = None
    var priorZ_0: Option[Array[Double]] = None

    var priorAlpha = Array[Double]()
    var priorBeta = Array[Double]()


    // E-step var
    var probZ_1 = Array[Double]()
    var probZ_0 = Array[Double]()

    // todo complete EM
    override var optimizer: Seq[LabeledData] => (Seq[Double], Seq[Double]) = _


    private def init(data: Seq[LabeledData]) = {
        // todo when workerNumber or entityNumber not be inited

        val uniform = Uniform(0, 1)
        alpha = Some(uniform.sample(workerNumber).toArray[Double])
        beta = Some(uniform.sample(entityNumber).toArray[Double])

        z_truth = Some(Array.fill(entityNumber)(0))

        priorZ_0 = Some(Array.fill(entityNumber)(0.5))
        priorZ_1 = Some(Array.fill(entityNumber)(0.5))

        priorAlpha = Array.fill(workerNumber)(1.0)
        priorBeta = Array.fill(entityNumber)(1.0)
    }


    private def computeE(data: Seq[LabeledData]) = {
        probZ_1 = priorZ_1.get.map(log(_))
        probZ_0 = priorZ_0.get.map(x => log(1.0 - x))

        // \prod p(l_{ij}| z_j, \alpha_i, \beta_j)
        data.foreach({ label =>
            val ability = alpha.get(label.person)
            val difficulty = beta.get(label.entity)
            val l = label.label.toInt

            // change prod to sum to avoid overflow
            probZ_1(label.entity) += logSigmod(l, 1, ability, difficulty)
            probZ_0(label.entity) += logSigmod(l, 0, ability, difficulty)
        })

        List.range(0, workerNumber).foreach(index => {
            probZ_0(index) = exp(probZ_1(index))
            probZ_1(index) = exp(probZ_0(index))

            probZ_0(index) = probZ_0(index) / (probZ_0(index) + probZ_0(index))
            probZ_1(index) = 1 - probZ_0(index)
        })

    }

    private def computeQ(data: Seq[LabeledData]) {
        // compute Q by \sum_j E[\ln p(z_j)] + \sum_{ij} E[\ln p(l_{ij}|z_j, \alpha_i, \beta_j)] + gauess prior
        var Q = 0.0

        // compute \sum_j E[\ln p(z_j)]
        Q += probZ_0.zip(priorZ_0.get).foldLeft(0.0)((y, x) => x._1 * log(1 - x._2) + y)
        Q += probZ_1.zip(priorZ_1.get).foldLeft(0.0)((y, x) => x._1 * log(x._2) + y)

        // compute \sum_{ij} E[\ln p(l_{ij}|z_j, \alpha_i, \beta_j)]
        data.foreach(label => {
            val person = label.person
            val entity = label.entity
            val l = label.label.toInt

            var ln_sigma = -log(1 + exp(alpha.get(person) * -exp(beta.get(entity))))

            if (ln_sigma.isNegInfinity) {
                ln_sigma = exp(beta.get(entity)) * alpha.get(person)
                logger.info(s"person $person labeled $entity is $l overflow")
            }

            var ln_minus_sigma = -log(1 + exp(exp(beta.get(entity)) + alpha.get(person)))

            if (ln_minus_sigma.isNegInfinity) {
                ln_minus_sigma = -exp(beta.get(entity) + alpha.get(person))
            }

            Q += probZ_1(entity) * (l * ln_sigma + (1 - l) * ln_minus_sigma) +
                probZ_0(entity) * (l * ln_minus_sigma + (1 -  l) * ln_sigma)
        })

        // add gaussian prior
        Q += alpha.get.zip(priorAlpha).foldLeft(0.0)((x, y) => x + zscore(y._1 - y._2))
        Q += beta.get.zip(priorBeta).foldLeft(0.0)((x, y) => x + zscore(y._1 - y._2))
    }

    def zscore(x: Double): Double = 1/sqrt(2 * Pi) * exp(-pow(x, 2) / 2)

    private def computeM(): Unit = {
        // todo compute M step

    }

    private def logSigmod(l: Int, z: Int, a: Double, b: Double): Double = {
        if (z == l) -log(1.0 + exp(a * exp(-b)))
        else 1 - log(1.0 + exp(1 * exp(-b)))
    }

    // todo createModel
    override protected def createModel(solution: Seq[Double], workers: Seq[Double]): OrdinaryCSModel = ???

    def setEpsilon(e: Double): this.type = {
        self.epsilon = e
        self
    }

    def setMaxIterations(m: Int): this.type = {
        self.maxIterations = m
        self
    }

    def setWorkerNumber(w: Int): this.type = {
        self.workerNumber = w
        self
    }

    def setEntityNumber(e: Int): this.type = {
        self.entityNumber = e
        self
    }
}

