package docman

import java.util.Properties

/** Load version string from `docman/version.properties`. */
case object VersionInfo {
  val properties = {
    val properties = new Properties()
      val stream = this.getClass.getResourceAsStream("version.properties")
      properties.load(stream)
      /* or properties.loadFromXML(...) */
      stream.close()
    properties
  }
  val version = properties.getProperty("version", "not found")
  val isDefVersion = version.contains("-")
}

