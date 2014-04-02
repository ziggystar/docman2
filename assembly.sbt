import AssemblyKeys._

assemblySettings

jarName in assembly := s"${name.value}-${version.value}.jar"

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
  case PathList("org", "apache", "commons", "collections", xs@_*) => MergeStrategy.last
  case x => old(x)
}
}

assemblyOption in assembly ~= { _.copy(prependShellScript = Some(defaultShellScript)) }
