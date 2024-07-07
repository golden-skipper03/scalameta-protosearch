val scala2Version = "2.13.14"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scalameta-demo",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala2Version,

    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0" % Test,
    libraryDependencies += "org.scalameta" %% "scalameta" % "4.9.7"
  )
