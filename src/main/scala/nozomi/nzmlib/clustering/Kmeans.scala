package nozomi.nzmlib.clustering

import breeze.linalg
import breeze.linalg.DenseVector
import nozomi.nzmlib.mlutil.DataSet._

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// todo unit test

/**
  * Created by sai on 16-3-7.
  */
class Kmeans(
              private var k: Int,
              private var maxIterations: Int,
              private var runs: Int,
              private var epsilon: Double,
              private var initializationStep: Int,
              private var seed: Long
            ) {


  private def initKmeansll(data: Seq[VectorWithNorm]): Array[Array[VectorWithNorm]] = {
    // 预先构造好运行次数的中心点,以及对应的costs,每次计算同时进行
    val centers = Array.tabulate(runs)(r => ArrayBuffer.empty[VectorWithNorm])
    var costs = data.map(_ => Array.fill(runs)(Double.PositiveInfinity))


    val seed = util.Random
    seed.setSeed(this.seed)

    val sample = data.simple(runs, seed.nextLong)
    require(sample.size >= runs, s"Required $runs samples but got ${sample.size}")
    val newCenters = Array.tabulate(runs)(r => ArrayBuffer(sample(r)))


    def mergeNewCenters(): Unit = {
      var r = 0
      while (r < runs) {
        centers(r) ++= newCenters(r)
        newCenters(r).clear()
        r += 1
      }
    }

    //
    var step = 0
    while (step < this.initializationStep) {
      val preCosts = costs

      //
      costs = data.zip(preCosts).map { case (point, cost) =>
        Array.tabulate(runs) { r =>
          math.min(Kmeans.pointCost(newCenters(r), point), cost(r))
        }
      }


      val sumCosts = costs.par.aggregate(new Array[Double](runs))(
        seqop = (s, v) => {
          var r = 0
          while (r < runs) {
            s(r) += v(r)
            r += 1
          }
          s
        },
        combop = (s0, s1) => {
          var r = 0
          while (r < runs) {
            s0(r) += s1(r)
            r += 1
          }
          s0
        }
      )

      val chosen = data.zip(costs).zipWithIndex.map {
        case ((p, c), index) =>
          val rand = new Random()
          rand.setSeed(seed.nextInt ^ (step << 16) ^ index)
          val rs = (0 until runs).filter { r =>
            rand.nextDouble() < 2.0 * c(r) * k / sumCosts(r)
          }

          if (rs.nonEmpty) Some((p, rs)) else None
      }
      mergeNewCenters()

      // todo Some is not correct
      chosen.foreach { case Some((p, rs)) =>
          rs.foreach(newCenters(_) += p)
      }

      step += 1

    }

    mergeNewCenters()

    // we may get more than k candidate centers for each run
    val weightMap = data.par.flatMap { p =>
      Iterator.tabulate(runs) {
        r => ((r, Kmeans.findClosest(centers(r), p)._1), 1.0)
      }
    }.groupBy(x => x._1).map {
      case (k, ys) => (k, ys.map(_._2).sum)
    }

    val findalCenters = (0 until runs).par.map {
      r =>
        val myCenters = centers(r).toArray
        val myWeights = myCenters.indices.map(i => weightMap.getOrElse((r, i), 0.0)).toArray
        Kmeans.KmeansPlusPlus(r, myCenters, myWeights, k, 30)
    }


    centers.map(x => x.toArray)


  }

}

/**
  * Vector with its norm for fast distance computation
  *
  * @param vector vector
  * @param norm   vector's norm
  */
private[clustering]
class VectorWithNorm(val vector: DenseVector[Double], val norm: Double) extends Serializable {
  def this(vector: DenseVector[Double]) = this(vector, linalg.norm(vector))

  def this(array: Array[Double]) = this(new DenseVector[Double](array))
}


object Kmeans {
  private[nzmlib] def pointCost(
                                 centers: TraversableOnce[VectorWithNorm],
                                 point: VectorWithNorm
                               ): Double =
    findClosest(centers, point)._2


  private[nzmlib] def findClosest(
                                   centers: TraversableOnce[VectorWithNorm],
                                   point: VectorWithNorm
                                 ): (Int, Double) = {
    var bestDistance = Double.PositiveInfinity
    var bestIndex = 0

    var i = 0

    centers.foreach { center =>
      // 使用 |a| - |b| 代替 |a - b|
      var lowerBoundOfSqDist = center.norm - point.norm
      lowerBoundOfSqDist = lowerBoundOfSqDist * lowerBoundOfSqDist
      if (lowerBoundOfSqDist < bestDistance) {
        val distance: Double = center.vector dot point.vector
        if (distance < bestDistance) {
          bestDistance = distance
          bestIndex = i
        }
      }
      i += 1
    }

    (bestIndex, bestDistance)
  }

  // todo Kmeans++
  def KmeansPlusPlus(
                    seed: Long,
                    points: Array[VectorWithNorm],
                    weights: Array[Double],
                    k: Int,
                    maxIterations: Int
                    ): Array[VectorWithNorm] = ???
}
