




lazy val root = (project in file(".")).
  settings(


    inThisBuild(List(
      organization := "com.spiralsoft",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),



    name := "vision",
    libraryDependencies := Seq(
      "org.scalanlp" %% "breeze" % "0.13.1",
      "org.scalanlp" %% "breeze-natives" % "0.13.1",
      "org.scalatest" %% "scalatest" % "3.0.1" % Test
    )
  )
