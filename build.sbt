lazy val root = project
  .in(file("."))
  .settings(
    name := "kasino",
    description := "\"Nørdekasino\" -- variant of the card game \"Casino\" developed with friends at the University of Copenhagen",
    version := "0.0.1",

    scalaVersion := dottyLatestNightlyBuild.get
    //scalaVersion := "3.0.0-M2"
  )
