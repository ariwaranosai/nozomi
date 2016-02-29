package nozomi.nzmlib.feature

import breeze.linalg.Matrix

/**
  * Created by ariwaranosai on 16/2/29.
  *
  */

class StandardScaler(withMean: Boolean, withStd: Boolean) {

    def this() = this(false, true)

    // TODO change to logging
    if (!(withMean || withStd)) {
        println("Warning: Both withMean and withStd are false. The model does nothing.")
    }

    def fit(data: Seq[Matrix[Double]]): StandardScalerModel = {
        val size = data.head.cols
        val e = Matrix.zeros[Double](1, size)
        val e_2 = Matrix.zeros[Double](1, size)
        val summary = data.foldLeft((e, e_2))((sum, x) => (sum._1 + x, sum._2 + (x :* x)))

        val len: Double = data.length
        val expection: Matrix[Double] = summary._1 :/ len
        val std: Matrix[Double] = ((summary._2 :/  len) - (expection :* expection)).toDenseMatrix.map(math.sqrt)

        new StandardScalerModel(
            std,
            expection,
            withStd,
            withMean
        )
    }

}


class StandardScalerModel (
                          val std: Matrix[Double],
                          val mean: Matrix[Double],
                          var withStd: Boolean,
                          var withMean: Boolean) {

    def this(std: Matrix[Double], mean: Matrix[Double]) {
        this(std, mean, withStd = std != null, withMean = mean != null)
        require(this.withMean || this.withStd,
            "at least one of std or mean must be provided")

        if (this.withMean && this.withStd) {
            require(mean.size == std.size,
                "mean and std must have the same size")
        }
    }

    def this(std: Matrix[Double]) = this(std, null)

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
      * @param data data point to tranform
      * @return
      */
    def transform(data: Matrix[Double]): Matrix[Double] = {
        require(mean.size == data.cols)
        require(data.rows == 1)

        if (withMean) {
            val localShift = shift
            val size = data.cols
            val values = data.copy

            if (withStd) {
                if (withStd) {
                    var i = 0
                    while(i < size) {
                        values((0, i)) = if (std((0, i)) != 0.0) (values((0, i)) - localShift(i)) * (1.0 / std(0, i)) else 0.0
                        i += 1
                    }
                } else {
                    var i = 0
                    while(i < size) {
                        values((0, i)) -= localShift(i)
                        i += 1

                    }
                }
            }
            values
        } else if (withStd) {
            var i = 0
            val size = data.cols
            val values = data.copy

            while(i < size) {
                values((0, i)) *= (if (std((0, i)) != 0.0) 1.0 / std((0, i)) else 0.0)
                i += 1
            }
            values
        } else {
            data
        }
    }
}
