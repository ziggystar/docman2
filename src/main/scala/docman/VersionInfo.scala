package docman

import java.util.Properties
import com.typesafe.scalalogging.slf4j.LazyLogging
import resource._

/** Load version string from `docman/version.properties`. */
case object VersionInfo extends LazyLogging {
  val versionNotFoundString: String = "unknown"
  val properties: Properties = {
    val properties = new Properties()
    //try to load resources from resource file
    Option(this.getClass.getResourceAsStream("version.properties")).fold{
      logger.warn("could not load version.properties as resource")
    }{ stream =>
      logger.debug("loading version.properties from resource")
      properties.load(stream)
      stream.close()
    }
    properties
  }
  val version: String = properties.getProperty("version", versionNotFoundString)
  val isDefVersion: Boolean = version.contains("-") | version == versionNotFoundString
}

