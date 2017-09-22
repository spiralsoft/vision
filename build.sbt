
bintrayOrganization := Some("spiralsoft")
bintrayRepository := "vision"
licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html"))

lazy val root = (project in file(".")).
  settings(


    inThisBuild(List(
      organization := "io.spiralsoft",
      name         := "vision.core",
      version      := "0.1.1-SNAPSHOT",
      scalaVersion := "2.12.1"
    )),

    libraryDependencies := Seq(
      "org.scalanlp" %% "breeze" % "0.13.1",
      "org.scalanlp" %% "breeze-natives" % "0.13.1",
      "org.scalatest" %% "scalatest" % "3.0.1" % Test
    )
  )


