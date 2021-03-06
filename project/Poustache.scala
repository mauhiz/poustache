import sbt.Keys._
import sbt._
import scoverage.ScoverageSbtPlugin._

object Poustache extends Build {

  lazy val root = Project(id = "poustache", base = file("."), settings = Seq(
    organization := "net.mauhiz",
    scalaVersion := "2.11.5",
    libraryDependencies ++= Seq(
      "org.parboiled" %% "parboiled" % "2.1.0",
      "org.json4s" %% "json4s-native" % "3.2.11",
      "org.scalatest" %% "scalatest" % "2.2.4" % Test
    ),
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xlint"),
    updateOptions := updateOptions.value
      .withCircularDependencyLevel(CircularDependencyLevel.Error)
      .withCachedResolution(true),
    ScoverageKeys.coverageHighlighting := true
  ))
}
