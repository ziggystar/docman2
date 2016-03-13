# Building

SBT is used to generate a resource file `docman/version.properties`. 
For development, make sure you get SBT to generate it (`sbt run`, as compile isn't enough), and tell your IDE about
the generated resources.

# Dev-Mode

The Dev-Mode uses a different `Preference` package to allow different settings (in particular different path to library).
This protects your library from damage caused by a development version.

    if(devMode) Preferences.userNodeForPackage(this.getClass).node("development")
    else        Preferences.userNodeForPackage(this.getClass)
       
It is triggered by the Version containing a `-` character.

    val isDefVersion = version.contains("-") | version == versionNotFoundString