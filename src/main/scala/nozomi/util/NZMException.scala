package nozomi.util

/**
  * Created by ariwaranosai on 16/2/29.
  *
  */

class NZMException(message: String, cause: Throwable)
    extends Exception(message, cause) {

    def this(message: String) = this(message, null)
}
