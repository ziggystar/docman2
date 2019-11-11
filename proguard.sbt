enablePlugins(SbtProguard)

proguardOptions in Proguard ++= Seq("-dontnote", "-ignorewarnings", "-dontobfuscate", "-dontoptimize")

proguardVersion in Proguard := "6.2.0"

proguardOptions in Proguard += ProguardOptions.keepMain("docman.frontend.cli.CLI")

proguardMerge in Proguard := true

javaOptions in (Proguard, proguard) := Seq("-Xmx1G")

proguardMergeStrategies in Proguard += ProguardMerge.discard("META-INF/.*".r)
