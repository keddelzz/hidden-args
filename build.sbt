/*
 * =================================== settings ===================================
 */

lazy val settings = Seq(
  name := "hidden-args",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.8",
  resolvers += Resolver.sonatypeRepo("releases"),
  addCompilerPlugin("org.scalamacros" %% "paradise" % "2.1.0" cross CrossVersion.full),
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
)

/*
 * =================================== projects ===================================
 */

lazy val hiddenargs = (project in file("."))
  .settings(settings :_*)
