package nozomi.nzmlib.clustering

import breeze.linalg
import breeze.linalg.DenseVector
import nozomi.nzmlib.mlutil.DataSet._
import nozomi.util.NZMLogging

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
            ) extends NZMLogging {


  private def initRandom(data: Seq[VectorWithNorm]): Array[Array[VectorWithNorm]] = {
    val sample = data.simple(runs * k, Random.nextLong())

    Array.tabulate(runs)(r => sample.slice(r * k, (r + 1) * k).map {
      v => VectorWithNorm(DenseVector(v.vector.data), v.norm)
    }.toArray)
  }

  /**
    * 采用 kmeans||的初始化方法
    * 论文 http://theory.stanford.edu/~sergei/papers/vldb12-kmpar.pdf
    * 具体解释可以看 http://stats.stackexchange.com/questions/135656/k-means-a-k-a-scalable-k-means
    *
    * @param data data to kmeans
    * @return centers vector of runs times
    */
  private def initKmeansll(data: Seq[VectorWithNorm]): Array[Array[VectorWithNorm]] = {
    logger.info(" initing ")

    // 预先构造好运行次数的中心点,以及对应的costs,每次计算同时进行
    val centers = Array.tabulate(runs)(r => ArrayBuffer.empty[VectorWithNorm])
    var costs = data.map(_ => Array.fill(runs)(Double.PositiveInfinity))

    val seed = util.Random
    seed.setSeed(this.seed)

    val sample = data.simple(runs, seed.nextLong)
    require(sample.size >= runs, s"Required $runs samples but got ${sample.size}")
    // 为每一次run,取样第一个点
    val newCenters = Array.tabulate(runs)(r => ArrayBuffer(sample(r)))

    def mergeNewCenters(): Unit = {
      var r = 0
      while (r < runs) {
        centers(r) ++= newCenters(r)
        newCenters(r).clear()
        r += 1
      }
    }

    // 这个地方可以指定,也可以通过计算那个\psi来得到
    var step = 0
    while (step < this.initializationStep) {
      val preCosts = costs

      // 对于每一个数据点,分别计算它在每个run中靠集合C,最近的点的距离。但是由于
      // 只有一个元素,所以就算一个就行了
      costs = data.zip(preCosts).par.map { case (point, cost) =>
        Array.tabulate(runs) { r =>
          math.min(Kmeans.pointCost(newCenters(r), point), cost(r))
        }
      }.to


      // 计算那个psi的sum,用作概率的归一化项
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

      // 随机选择 令l=2*k
      val chosen = data.zip(costs).par.zipWithIndex.map {
        case ((p, c), index) =>
          val rand = new Random()
          rand.setSeed(seed.nextInt ^ (step << 16) ^ index)
          val rs = (0 until runs).filter { r =>
            rand.nextDouble() < 2.0 * c(r) * k / sumCosts(r)
          }

          if (rs.nonEmpty) Some((p, rs)) else None
      }

      // 把newCenters里面的加到centers里面去
      mergeNewCenters()

      // 把新选出来的newCenters加到centers里面去
      chosen.foreach { case Some((p, rs)) =>
        rs.foreach(newCenters(_) += p)
      case None =>
      }

      step += 1

    }

    mergeNewCenters()

    // 这样每一次run都有很多的
    val weightMap = data.flatMap { p =>
      Iterator.tabulate(runs) {
        r => ((r, Kmeans.findClosest(centers(r), p)._1), 1.0)
      }
    }.groupBy(x => x._1).map {
      case (k, ys) => (k, ys.map(_._2).sum)
    }

    logger.info(" in kmeans++ ")

    val finalCenters = (0 until runs).map {
      r =>
        val myCenters = centers(r).toArray
        val myWeights = myCenters.indices.map(i => weightMap.getOrElse((r, i), 0.0)).toArray
        Kmeans.KmeansPlusPlus(r, myCenters, myWeights, k, 30)
    }

    finalCenters.toArray
  }

  private var initialModel: Option[KmeansModel] = None

  def setInitialModel(model: KmeansModel): this.type = {
    require(model.k == k, " mismatched cluster count")
    initialModel = Some(model)
    this
  }


}

/**
  * Vector with its norm for fast distance computation
  *
  * @param vector vector
  * @param norm   vector's norm
  */
private[clustering]
case class VectorWithNorm(vector: DenseVector[Double], norm: Double) extends Serializable {
  def this(vector: DenseVector[Double]) = this(vector, linalg.norm(vector))
  def this(array: Array[Double]) = this(new DenseVector[Double](array))

  override def toString: String = vector.toString()
}


object Kmeans extends NZMLogging {
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

  private[clustering]
  def KmeansPlusPlus(
                      seed: Long,
                      points: Array[VectorWithNorm],
                      weights: Array[Double],
                      k: Int,
                      maxIterations: Int
                    ): Array[VectorWithNorm] = {


    val rand = new Random(seed)
    val dim: Int = points(0).vector.length

    val centers = new Array[VectorWithNorm](k)

    centers(0) = pickWeighted(rand, points, weights)
    // 预先计算每一个点到第一个点的距离
    val costArray = points.map(x => x.vector.dot(centers(0).vector))

    for (i <- 1 until k) {
      // 取随机数
      val sum = costArray.zip(weights).map(p => p._1 * p._2).sum
      val r = rand.nextDouble() * sum
      var cumulativeScore = 0.0
      var j = 0
      while (j < points.length && cumulativeScore < r) {
        cumulativeScore += weights(j) * costArray(j)
        j += 1
      }

      if (j == 0) {
        logger.warn("kmeans++ initialization ran out of disinct points for centers." +
          s" Using deplicate point for k = $i.")
        centers(i) = points(0)
      } else {
        centers(i) = points(j - 1)
      }

      for (p <- points.indices) {
        costArray(p) = math.min(points(p).vector.dot(centers(i).vector), costArray(p))
      }
    }

    // Lloyd's algorithm kmeans
    val oldClosest = Array.fill(points.length)(-1)
    var iteration = 0
    var moved = true
    while (moved && iteration < maxIterations) {
      moved = false
      val counts = Array.fill(k)(0.0)
      val sums = Array.fill(k)(DenseVector.zeros[Double](dim))
      var i = 0
      while (i < points.length) {
        val p = points(i)
        val index = Kmeans.findClosest(centers, p)._1
        sums(index) += weights(i) * p.vector
        counts(index) += weights(i)
        if (index != oldClosest(i)) {
          moved = true
          oldClosest(i) = index
        }

        i += 1
      }

      // 更新中心点
      var j = 0
      while (j < k) {
        if (counts(j) == 0.0) {
          centers(j) = points(rand.nextInt(points.length))
        } else {
          sums(j) *= 1.0 / counts(j)
        }
        j += 1
      }

      iteration += 1
    }

    if (iteration == maxIterations) {
      logger.info(s" kmeans++ reached the max number of iterations: $maxIterations.")
    } else {
      logger.info(s" kmeans++ converged in $iteration iterations.")
    }

    centers
  }

  private def pickWeighted[T](rand: Random, data: Array[T], weights: Array[Double]): T = {
    val r = rand.nextDouble() * weights.sum
    var i = 0
    var curWeight = 0.0
    while (i < data.length && curWeight < r) {
      curWeight += weights(i)
      i += 1
    }
    data(i - 1)
  }
}
