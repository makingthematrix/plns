// Comment to get more information during initialization
logLevel := Level.Warn

scalaVersion := "2.10.0"

// The Typesafe repository 
resolvers += "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"

//resolvers += "Objectify Play Repository" at "http://schaloner.github.io/releases/"

// Use the Play sbt plugin for Play projects
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.2.0")

//libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.2.1", "org.scala-lang" %% "scala-library" % "2.10")