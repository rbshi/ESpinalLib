import sbt.Keys.libraryDependencies

ThisBuild / version := "1.0"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / organization := "org.example"

val spinalVersion = "1.6.1"
val spinalCore = "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion
val spinalLib = "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion
val spinalIdslPlugin = compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion)

lazy val mylib = (project in file("."))
  .settings(
    name := "espinallib",
    libraryDependencies ++= Seq(spinalCore, spinalLib, spinalIdslPlugin),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % Test,
    libraryDependencies ++= Seq(
        "com.lihaoyi" %% "os-lib" % "0.8.0",
        "org.scodec" %% "scodec-bits" % "1.1.12",
        "org.scodec" %% "scodec-core" % "1.11.4",
    )
  )

fork := true
