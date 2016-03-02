package nozomi.nzmlib.mlutil

/**
  * Created by ariwaranosai on 16/3/2.
  *
  */

trait Saveable {

    /**
      * @param path path to save model
      */
    def save(path: String): Unit

    protected def formatVersion: String
}

trait Loader[M <: Saveable] {

    /**
      * @param path load model for saved
      * @return
      */
    def load(path: String): M

}
