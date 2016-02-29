package nozomi.nzmlib.regression

import breeze.linalg.Matrix
import nozomi.nzmlib.optimization.Optimizer
import nozomi.util.NZMException

/**
  * Created by ariwaranosai on 16/2/29.
  *
  */

/**
  * :: GeneralizedLinearModel ::
  * GeneralizedLinearModel is abstract Model representing model using
  * GeneralizedLinearAlgorith. It consist of a weight vector and a
  * intercept.
 *
  * @param weights Weights for every feature.
  * @param intercept Intercept for this model.
  */
abstract class GeneralizedLinearModel (
    val weights: Matrix[Double], val intercept: Double) {

    /**
      * Predict values for this given data point
      *
      * @param data Row vector containing the feature for this data point
      * @param weights Column vector containing the weights of the model
      * @param intercept Intercept of the model
      * @return
      */
    protected def predictPoint(data: Matrix[Double], weights: Matrix[Double], intercept: Double): Double

    /**
      * Predict values for the given data set using the model trained with parallelization
 *
      * @param testData input data
      * @return
      */
    def predict_par(testData: Seq[Matrix[Double]]) = {
        testData.par.map(point => predictPoint(point, weights, intercept))
    }

    /**
      * Predict values for the given data set using the model trained without parallelization
 *
      * @param testData input data
      * @return
      */
    def predict(testData: Iterable[Matrix[Double]]) = {
        testData.map(point => predictPoint(point, weights, intercept))
    }

    /**
      * predict value for a single data point
      */
    def predict(testData: Matrix[Double]) = predictPoint(testData, weights, intercept)

    /**
      * Print a summary of the model
      */
    override def toString: String = {
        s"${this.getClass.getName}: intercept = $intercept, numFeatures=${weights.size}"
    }
}


abstract class GeneralizedLinearAlgorithm[M <: GeneralizedLinearModel] {

    /**
      * The optimizer to solve the problem
      *
      */
    def optimizer: Optimizer

    protected val validators: Seq[Seq[LabeledPoint] => Boolean] = List()

    /** add intercept or not **/
    protected var addIntercept: Boolean = false

    /** validate or not **/
    protected var validateData: Boolean = true

    /** used for some multi-line model such as multi-logistic regression **/
    protected var numOfLinearPredictor: Int = 1

    /**
      * which to scaling before model or not
      * scaling will help optimizer converging faster
      */
    private[nzmlib] var useFeatureScaling = false

    protected var numFeatures: Int = -1

    def getNumFeatures: Int = this.numFeatures

    private[nzmlib] def setFeaturesScaling(useFeatureScaling: Boolean): this.type = {
        this.useFeatureScaling = useFeatureScaling
        this
    }

    protected def createModel(weights: Matrix[Double], intercept: Double): M

    def isAddIntercept: Boolean = this.addIntercept

    def setIntercept(addIntercept: Boolean): this.type = {
        this.addIntercept = addIntercept
        this
    }

    def setValidateData(validateData: Boolean): this.type = {
        this.validateData = validateData
        this
    }

    /**
      * Generate the inital weights when the user does not supply them
      *
      * @param input input data set
      * @return
      */
    protected def generateInitialWeights(input: Seq[LabeledPoint]): Matrix[Double] = {
        if (numFeatures < 0) {
            numFeatures = input.map(_.features.size).head
        }

        if (numOfLinearPredictor == 1) {
            Matrix.zeros[Double](numFeatures, 1)
        } else if (addIntercept) {
            Matrix.zeros[Double]((numFeatures + 1) * numOfLinearPredictor, 1)
        } else {
            Matrix.zeros[Double](numFeatures * numOfLinearPredictor, 1)
        }
    }

    // TODO complete run and try linear

//    def run(input: Seq[LabeledPoint]): M = {
//        run(input, generateInitialWeights(input))
//    }

//    def run(input: Seq[LabeledPoint], initialWeights: Matrix[Double]): M = {
//        if (numFeatures < 0) {
//            numFeatures = input.map(_.features.size).head
//        }
//
//        // TODO give more detail
//        if (validateData && !validators.forall(func => func(input))) {
//            throw new NZMException("Input validation failed")
//        }
//
//        val scaler = if (useFeatureScaling) {
//
//        } else {
//            null
//        }
//
//    }

}