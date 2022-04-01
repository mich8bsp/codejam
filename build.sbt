ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val codejam2020 = (project in file("codejam2020"))
  .settings(
    name := "codejam2020"
  )

lazy val codejam2021 = (project in file("codejam2021"))
  .settings(
    name := "codejam2021"
  )

lazy val codejam2022 = (project in file("codejam2022"))
  .settings(
    name := "codejam2022"
  )

lazy val root = (project in file("."))
  .settings(
    name := "codejam"
  )
