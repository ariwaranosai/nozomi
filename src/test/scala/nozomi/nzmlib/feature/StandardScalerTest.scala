package nozomi.nzmlib.feature

import breeze.linalg.DenseVector
import org.scalatest.FlatSpec

/**
  * Created by ariwaranosai on 16/3/1.
  *
  */

class StandardScalerTest extends FlatSpec {

    val seq = {
        val a = DenseVector[Double](1,2,3,4,5)
        val b = DenseVector[Double](2,3,4,5,6)
        val c = DenseVector[Double](3,4,5,6,7)

        Array(a, b, c)
    }

    val epsilon = 0.000000000001


    "withMean" should "get correct means" in {
        // TODO use scalacheck to generate test

        val scaler = new StandardScaler(true, false)

        assert(scaler.fit(seq).mean == DenseVector[Double](2,3,4,5,6))
    }

    "withStd" should "get correct std" in {
        val scaler = new StandardScaler(false, true)

        val k: Double = math.sqrt((1.0 + 4.0 + 9.0) / 3.0 - 4.0)

        scaler.fit(seq).std.foreach(
            x => assert(math.abs(x - k) < epsilon)
        )
    }

    "tranform WithMean" should "mins means" in {
        val scaler = new StandardScaler(true, false).fit(seq)

        val t = scaler.transform(DenseVector[Double](1.2,3.9,5.4,6.8,9))
        val d = DenseVector[Double](-0.8, 0.9, 1.4,1.8, 3)

        t.toArray.zip(d.toArray).foreach(x =>
            assert(x._1 - x._2 < epsilon)
        )

    }

    "tranform withStd" should "div std" in {
        val scaler = new StandardScaler(false, true).fit(seq)

        val t = scaler.transform(DenseVector[Double](1.2,3.9,5.4,6.8,9))
        val d = t :/ scaler.std

        t.toArray.zip(d.toArray).foreach(x =>
            assert(x._1 - x._2 < epsilon)
        )
    }

    "tranform with mean and std" should "mins means and div std" in {
        val scaler = new StandardScaler(true, true).fit(seq)

        intercept[IllegalArgumentException] {
            val t = scaler.transform(DenseVector[Double](1.2,3.9,5.4,6.8))
        }
    }
}
