package nozomi.nzmlib.clustering

/**
  * Created by sai on 16-3-7.
  */
class Kmeans (
             private var k: Int,
             private var maxIterations: Int,
             private var runs: Int,
             private var initializationMode: String,
             private var initializationSteps: Int,
             private var epsilon: Double,
             private var seed: Long
             ) {

}
