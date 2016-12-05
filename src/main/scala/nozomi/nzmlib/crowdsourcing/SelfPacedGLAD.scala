package nozomi.nzmlib.crowdsourcing

import breeze.numerics._
import breeze.stats.distributions.Uniform
import nozomi.util.NZMLogging

/**
  * Created by ariwaranosai on 2016/12/5.
  *
  */

class SelfPacedGLAD extends GeneralizedCSAlgorithm[GLADModel]
  with NZMLogging { self =>

    // some const
    val THRESHOLD: Double = 1E-5


    // some var
    var epsilon: Double = 0.5
    var maxIterations: Int = 1000

    // labeler's ability
    var alpha: Option[Array[Double]] = None
    // object's difficulty
    var beta: Option[Array[Double]] = None

    // prior
    var priorZ_1: Option[Array[Double]] = None
    var priorZ_0: Option[Array[Double]] = None

    var priorAlpha = Array[Double]()
    var priorBeta = Array[Double]()

    // E-step
    var probZ1 = Array[Double]()
    var probZ0 = Array[Double]()

    // M-step
    var qdAlpha = Array[Double]()
    var qdBeta = Array[Double]()

    // wight
    var weight = Array[Double]()
    var weightDelta = 0.5
    var selectK = 2

    private def init(data: Seq[LabeledData]) = {
        val uniform = Uniform(0, 1)
        assert(workerNum > 0)
        assert(entityNum > 0)
        alpha = Some(uniform.sample(workerNum).toArray[Double])
        beta = Some(uniform.sample(entityNum).toArray[Double])

        priorZ_0 = Some(Array.fill(entityNum)(0.5))
        priorZ_1 = Some(Array.fill(entityNum)(0.5))

        priorAlpha = Array.fill(workerNum)(1.0)
        priorBeta = Array.fill(entityNum)(1.0)
        logger.info(s"WGLAD: init data with $workerNum works and $entityNum entities")
    }

    private def initWithMajorVoting(data: Seq[LabeledData]) = {
        val uniform = Uniform(0, 1)
        assert(workerNum > 0)
        assert(entityNum > 0)

        priorZ_0 = Some(Array.fill(entityNum)(0.5))
        priorZ_1 = Some(Array.fill(entityNum)(0.5))

        priorAlpha = Array.fill(workerNum)(1.0)
        priorBeta = Array.fill(entityNum)(1.0)
        logger.info(s"WGLAD: init data with $workerNum works and $entityNum entities")

        // major voting
        val mv = new Voting().setDefaultLabel(-1).run(data)
        alpha = Some(mv.workers.toArray)
        // todo init beta with dif
        beta = Some(uniform.sample(entityNum).toArray)
    }

    override var optimizer: (Seq[LabeledData]) => (Seq[Double], Seq[Double]) = data => {
        logger.info("WGLAD: started")
        init(data)

        var Q = 0.0
        var lastQ = 0.0

        computeEWithW(data)
        Q = computeQWithW(data)
        logger.info(s"WGLAD: Q = $Q")

        var count = 1
        do {
            lastQ = Q
            computeEWithW(data)
            Q = computeQWithW(data)

            logger.info(s"WGLAD: after E-step Q = $Q")
            computeMWithW(data, 0.001, 0.1)
            Q = computeQWithW(data)
            logger.info(s"WGLAD: after M-step Q = $Q")

            updateW()
            count += 1
        } while(abs(Q - lastQ) > this.epsilon && count < maxIterations)
    }

    private def logSigmod(l: Int, z: Int, a: Double, b: Double): Double = {
        if (z == l) -math.log(1.0 + exp(a * exp(-b)))
        else 1 - log(1.0 + exp(1 * exp(-b)))
    }

    override protected def createModel(solution: Seq[Double], workers: Seq[Double]): GLADModel = {
        new GLADModel(solution, workers)
    }

    @inline private def getW(index: Int): Double = {
      1
    }

    def computeEWithW(data: Seq[LabeledData]): Unit = {
        probZ1 = priorZ_1.get.map(math.log)
        probZ0 = priorZ_0.get.map(x => math.log(1.0 - x))

        // \prod w_ij p(l_{ij}| z_j, \alpha_i, \beta_j)
        data.zipWithIndex.foreach { point =>
          val label = point._1
          val ability = alpha.get(label.person)
          val difficulty = beta.get(label.entity)
          val l = label.label.toInt

          // change prod to sum to avoid overflow
          val w = getW(point._2)
          probZ1(label.entity) += (math.log(w) + logSigmod(l, 1, ability, difficulty))
          probZ0(label.entity) += (math.log(w) + logSigmod(l, 0, ability, difficulty))
        }

        // renormalize
        Array.range(0, workerNum).foreach { index =>
          probZ0(index) = exp(probZ0(index))
          probZ1(index) = exp(probZ1(index))

          probZ0(index) = probZ0(index) / (probZ0(index) + probZ0(index))
          probZ1(index) = 1 - probZ1(index)
        }
    }

    def zScore(x: Double): Double = 1 / sqrt(2 * math.Pi) * exp(-pow(x, 2)/2)

    def computeQWithW(data: Seq[LabeledData]): Double = {
        // compute Q by \sum_j E[\ln p(z_j)] + \sum_{ij} E[\ln p(l_{ij} | z_j, \alpha_i, \beta_j)] + gauess prior
        var Q = 0.0
        // compute \sum_j E[\ln p(z_j)] with W
        Q += probZ0.zip(priorZ_0.get)
          .zipWithIndex.foldLeft(0.0)((start, point) => {
          getW(point._2) * point._1._1 * log(1.0 - point._1._2) + start
        })

        Q += probZ1.zip(priorZ_1.get)
          .zipWithIndex.foldLeft(0.0)((start, point) => {
            getW(point._2) * point._1._1 * log(point._1._2) + start
        })

        // compute \sum_{ij} E[\n p(l_{ij} | z_j, \alpha_i, \beta_j)] with W
        data.zipWithIndex foreach { point =>
          val label = point._1
          val person = label.person
          val entity = label.entity
          val l = label.label.toInt

          var lnSigma: Double = -log(1 + exp(alpha.get(person)) * -exp(beta.get(entity)))

          if (lnSigma isNegInfinity) {
              lnSigma = exp(beta.get(entity)) * alpha.get(person)
              logger.info(s"person $person labeled $entity is $l overflow")
          }

          var lnMinusSigma: Double = log(1 + exp(exp(beta.get(entity)) + alpha.get(person)))

          if (lnMinusSigma isNegInfinity) {
              lnMinusSigma = -exp(beta.get(entity) + alpha.get(person))
              logger.info(s"person $person labeled $entity is $l overflow in ln mins sigma")
          }

          Q += getW(point._2) * (probZ1(entity) * (l * lnMinusSigma + (1 - l) * lnMinusSigma) +
            probZ0(entity) * (l * lnMinusSigma + (1 - l) * lnMinusSigma))

        }

        Q += alpha.get.zip(priorAlpha).foldLeft(0.0)((start, point) => {
          start + zScore(point._1 - point._2)
        })

        Q += beta.get.zip(priorBeta).foldLeft(0.0)((start, point) => {
            start + zScore(point._1 - point._2)
        })

        Q
    }

    def computeMWithW(data: Seq[LabeledData], stepSize: Double, delta: Double) = {
        var iteration = 0
        var oldQ = computeQWithW(data)
        var newQ = oldQ
        do {
            oldQ = newQ
            gradientAscend(data, stepSize)
            newQ = computeQWithW(data)
            iteration += 1
        } while(iteration < maxIterations && newQ - oldQ > delta)
    }

    private def gradientAscend(data: Seq[LabeledData], stepSize: Double): Unit = {
        // add prior
        (0 until workerNum).foreach(i => qdAlpha(i) = alpha.get(i) - priorAlpha(i))
        (0 until entityNum).foreach(i => qdBeta(i) = beta.get(i) - priorBeta(i))

        data.zipWithIndex.foreach(point => {
            val j = point._1.entity
            val i = point._1.person
            val lij = point._1.label
            val index = point._2

            val sigma = sigmoid(beta.get(j) * alpha.get(i))

            qdAlpha(i) += getW(index) * (probZ1(j) * (lij - sigma) +
              probZ0(j) * (1 - lij - sigma) * exp(beta.get(j)))

            qdBeta(j) += getW(index) * (probZ1(j) * (lij - sigma) +
              probZ0(j) * (1 - lij - sigma) * alpha.get(i) * exp(beta.get(j)))
        })
        // todo ascent
    }

    def updateW() = ???


}
