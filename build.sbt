/*
 * =================================== settings ===================================
 */

lazy val settings = Seq(
  name := "hidden-args",
  organization := "com.github.keddelzz.hidden-args",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.12.1",
  crossScalaVersions := Seq("2.12.1", "2.11.8"),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),
  libraryDependencies ++= Seq(
    "org.scala-lang" %  "scala-reflect" % scalaVersion.value,
    "org.scalatest"  %% "scalatest"     % "3.0.0"  			  % "test",
    "com.chuusai"    %% "shapeless"     % "2.3.2" 			  % "test"
  )
)

bintrayReleaseOnPublish in ThisBuild := false

/*
 * ==================================== tasks =====================================
 */

addCommandAlias("testAll", ";test;examples/test")

/*
 * =================================== projects ===================================
 */

lazy val hiddenargs = (project in file("."))
  .settings(settings :_*)

lazy val examples = (project in file("examples"))
  .settings(settings :_*)
  .settings(
    name := "hidden-args-examples"
  )
  .dependsOn(hiddenargs) 
