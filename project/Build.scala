import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "PlayTranslate"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
		  "postgresql" % "postgresql" % "8.4-702.jdbc4",
		  jdbc, anorm
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
    resolvers := Seq("typesafe" at "http://repo.typesafe.com/typesafe/repo")
  )

}
