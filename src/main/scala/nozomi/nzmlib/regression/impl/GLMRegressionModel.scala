package nozomi.nzmlib.regression.impl

import java.io.PrintWriter

import org.json4s._
import org.json4s.JsonDSL.WithDouble._
import org.json4s.native.JsonMethods._
import breeze.linalg.DenseVector

import scala.io.Source

/**
  * Created by ariwaranosai on 16/3/2.
  *
  */

private[regression] object GLMRegressionModel {

    object SaveLoad {

        def formatVersion: String = "0.1"

        case class Data(weights: DenseVector[Double], intercept: Double)

        def save(path: String,
                 modelClass: String,
                 weights: DenseVector[Double],
                 intercept: Double) = {

            val modeldata = ("modelClass" -> modelClass) ~ ("formatVersion" -> formatVersion) ~
                ("data" -> (("weights" -> weights.toArray.toList) ~ ("intercept" -> intercept)))

            val modelString = compact(render(modeldata))

            Some(new PrintWriter(path)).foreach{p => p.write(modelString); p.close()}
        }

        def load(path: String, modelClass: String, numFeatures: Int): Data = {
            val dataString = Source.fromFile(path).getLines().map(_.stripLineEnd).mkString(" ")
            val mdata = parse(dataString)

            val mclass = (mdata \ "modelClass" \\ classOf[JString])(0)
            val format = (mdata \ "formatVersion" \\ classOf[JString])(0)

            assert(mclass == modelClass, "model class unexpected")
            assert(formatVersion == format, "format version unexpected")

            val weights: List[Double] = for {
                JObject(t) <- mdata
                JField("data", JObject(w)) <- t
                JField("weights", JArray(v)) <- w
                JDouble(x) <- v
            } yield x

            assert(weights.length == numFeatures, s"Expected $numFeatures, buf found ${weights.length} features" +
                s" when load $modelClass for $path")

            val intercept = (mdata \ "data" \ "intercept" \\ classOf[JDouble])(0)

            Data(DenseVector[Double](weights.toArray), intercept)

        }

    }

}
