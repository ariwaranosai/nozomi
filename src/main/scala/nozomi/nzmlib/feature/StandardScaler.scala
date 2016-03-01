package nozomi.nzmlib.feature

import breeze.linalg.DenseVector
import nozomi.util.NZMLogging

/**
  * Created by ariwaranosai on 16/2/29.
  *
  */

class StandardScaler(withMean: Boolean, withStd: Boolean) extends NZMLogging {

    def this() = this(false, true)

    if (!(withMean || withStd)) {
        logger.warn("Both withMean and withStd are false. The model does nothing.")
    }

    def fit(data: Seq[DenseVector[Double]]): StandardScalerModel = {
        val size = data.head.length
        val e = DenseVector.zeros[Double](size)
        val e_2 = DenseVector.zeros[Double](size)
        val summary = data.foldLeft((e, e_2))((sum, x) => (sum._1 + x, sum._2 + (x :* x)))

        val len: Double = data.length
        val expection: DenseVector[Double] = summary._1 :/ len
        val std: DenseVector[Double] = ((summary._2 :/ len) - (expection :* expection)).map(math.sqrt)

        new StandardScalerModel(
            std,
            expection,
            withStd,
            withMean
        )
    }

}


class StandardScalerModel (
                          val std: DenseVector[Double],
                          val mean: DenseVector[Double],
                          var withStd: Boolean,
                          var withMean: Boolean) {

    def this(std: DenseVector[Double], mean: DenseVector[Double]) {
        this(std, mean, withStd = std != null, withMean = mean != null)
        require(this.withMean || this.withStd,
            "at least one of std or mean must be provided")

        if (this.withMean && this.withStd) {
            require(mean.size == std.size,
                "mean and std must have the same size")
        }
    }

    def this(std: DenseVector[Double]) = this(std, null)

    def setWithMean(withMean: Boolean): this.type = {
        require(!(withMean && this.mean == null),
            "cannot set withMean to true while mean is null")
        this.withMean = withMean
        this
    }

    def setWithStd(withStd: Boolean): this.type = {
        require(!(withStd && this.std == null),
            "cannot set withStd to true while std is null")
        this.withStd = withStd
        this
    }

    private lazy val shift: Array[Double] = mean.iterator.map(_._2).toArray

    /**
      * tranform a single data point
 *
      * @param data data point to tranform
      * @return
      */
    def transform(data: DenseVector[Double]): DenseVector[Double] = {
        require(mean.size == data.length)

        if (withMean) {
            val localShift = shift
            val size = data.length
            val values = data.copy

            if (withStd) {
                var i = 0
                while(i < size) {
                    values(i) = if (std(i) != 0.0) (values(i) - localShift(i)) * (1.0 / std(i)) else 0.0
                    i += 1
                }
            } else {
                var i = 0
                while(i < size) {
                    values(i) -= localShift(i)
                    i += 1
                }
            }
            values
        } else if (withStd) {
            var i = 0
            val size = data.length
            val values = data.copy

            while(i < size) {
                values(i) *= (if (std(i) != 0.0) 1.0 / std(i) else 0.0)
                i += 1
            }
            values
        } else {
            data
        }
    }
}
