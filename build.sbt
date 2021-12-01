val scala3Version = "3.1.0"

scalacOptions ++= Seq(
  "-Yexplicit-nulls",
  "-Ysafe-init"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc2021",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
