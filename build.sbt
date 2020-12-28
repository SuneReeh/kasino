lazy val root = project
  .in(file("."))
  .settings(
    name := "kasino",
    description := "\"Nørdekasino\" -- variant of the card game \"Casino\" developed with friends at the University of Copenhagen",
    version := "0.2.0",

    //scalaVersion := dottyLatestNightlyBuild.get,
    scalaVersion := "3.0.0-M3",
    useScala3doc := true,
  )

mainClass in assembly := Some("kasino.Main")

libraryDependencies += ("com.typesafe.akka" %% "akka-actor-typed" % "2.6.10").withDottyCompat(scalaVersion.value)

libraryDependencies += ("org.scalatest" %% "scalatest" % "3.2.3" % "test")//.withDottyCompat(scalaVersion.value)
libraryDependencies += ("org.scalactic" %% "scalactic" % "3.2.3")//.withDottyCompat(scalaVersion.value)
//libraryDependencies += "org.scalamock" %% "scalamock" % "5.1.0" % "Test"
