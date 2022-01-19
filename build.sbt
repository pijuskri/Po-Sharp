ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "Compiler-asm",
    //libraryDependencies +=  // SBT
  )
ThisBuild / libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "2.3.3",
  "org.reflections" % "reflections" % "0.10.2",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.slf4j" % "slf4j-simple" % "1.7.33" % Runtime
)