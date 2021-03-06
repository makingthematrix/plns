import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "PlayTranslate"
  val appVersion      = "0.1"

  val appDependencies = Seq(
		  
		  "postgresql" % "postgresql" % "9.1-901-1.jdbc4",
		  jdbc, anorm
  )
          

  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
    resolvers := Seq("typesafe" at "http://repo.typesafe.com/typesafe/repo")
  )

}
