package docman.utils

import org.slf4j.{Logger, LoggerFactory}

trait Logging {outer =>
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
}
