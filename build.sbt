
name := "searchcriteria"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "upickle" % "0.4.0",
  "com.lihaoyi" %% "utest" % "0.4.3" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")