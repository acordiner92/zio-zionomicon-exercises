val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "zionomicon-excerises",
    version := "1.0.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "dev.zio" %% "zio" % "2.0.0-RC1"
    )
  )
