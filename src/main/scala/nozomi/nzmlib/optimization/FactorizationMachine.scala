package nozomi.nzmlib.optimization

import breeze.linalg.{Matrix, Vector}

/**
  * Created by ariwaranosai on 16/6/14.
  *
  */

class FactorizationMachineModel(w0:Double,
                            w: Vector[Double],
                            v: Matrix[Double]
                           ) {

    type Label = Int

    lazy val k: Int = v.cols
    lazy val p: Int = v.rows


    def evalFMValue(point: Vector[Double]): Double = {
        assert(point.size == w.size, "data and weight dim should the same")
        // compute \sum_{j = 1}^p w_j x_j + w_0
        val order1 =  (w.t * point) + w0
        // compute others
        // follow paper
        val order2 = (0 until  k).par.map(f => {
            val square_x = math.pow((0 until p).par.map(j => {
                v(j, f) * point(j)
            }).sum, 2)
            val square_sum = (0 until p).par.map( j => math.pow(v(j, f), 2) * math.pow(point(j), 2)).sum

            square_x - square_sum
        }).sum * 0.5

        order1 + order2
    }
}