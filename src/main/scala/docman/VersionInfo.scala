package docman

import com.typesafe.scalalogging.LazyLogging


/** Load version string from `docman/version.properties`. */
case object VersionInfo extends LazyLogging {
  val version: String = BuildInfo.version
  val isDefVersion: Boolean = version.contains("-")
}

