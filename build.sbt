name := "musa_2_0"

version := "0.1"

scalaVersion := "2.12.7"
//scalaVersion := "2.13.0"

scalacOptions in Compile += "-deprecation"
scalacOptions in Compile += "-feature"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.17"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"

libraryDependencies += "junit" % "junit" % "4.12" % Test

libraryDependencies += "mysql" % "mysql-connector-java" % "8.0.13" % Runtime

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"

libraryDependencies += "log4j" % "log4j" % "1.2.17"

libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.1.0" % Test







