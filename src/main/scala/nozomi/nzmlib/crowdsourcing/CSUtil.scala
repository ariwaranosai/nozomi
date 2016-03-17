package nozomi.nzmlib.crowdsourcing

import breeze.linalg.DenseMatrix

/**
  * Created by ariwaranosai on 16/3/17.
  *
  */

object CSUtil {

    /**
      * change labeledData to Matrix
      * @param data LabeledData in Seq
      * @return matrix contains label in (entiry_id, persion_id)
      */
    def labelData2Matrix(data: Seq[LabeledData], rows: Int, cols: Int, default: => Double): DenseMatrix[Double] = {
        var matrix = DenseMatrix.fill[Double](rows, cols)(default)

        data.foreach(x => {
            matrix.update(x.entity, x.person, x.label)
        })

        matrix
    }

}
