package nozomi.util

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

/**
  * Created by ariwaranosai on 16/3/1.
  *
  */

trait NZMLogging {
    private var _log: Logger = null

    private def initializeIfNecessary(): Unit = {
        // add for other backend
    }

    protected def logger: Logger = {
        if (_log == null) {
            initializeIfNecessary()
            var className = this.getClass.getName
            if (className.endsWith("$")) {
                className = className.substring(0, className.length - 1)
            }
            _log = Logger(LoggerFactory.getLogger(className))
        }

        _log
    }
}

