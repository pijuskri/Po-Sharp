

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.7"

lazy val root = (project in file("."))
  .settings(
    name := "Compiler-asm",
    //libraryDependencies +=  // SBT
  )
libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "2.3.3"
)