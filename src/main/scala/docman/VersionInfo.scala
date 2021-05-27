package docman

import docman.utils.Logging

/** Load version string from `docman/version.properties`. */
case object VersionInfo extends Logging {
  val version: String = BuildInfo.version
  val isDefVersion: Boolean = version.contains("-")
}

