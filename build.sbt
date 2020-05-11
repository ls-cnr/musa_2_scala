name := "musa_2_0"

version := "0.1"

scalaVersion := "2.12.7"

//scalacOptions in Compile += "-deprecation"
//scalacOptions in Compile += "-feature"

//resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies += "com.typesafe.akka" %% "akka-cluster-typed" % "2.6.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.1.1"

//libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.1.0" % Test
libraryDependencies += "junit" % "junit" % "4.12" % Test
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"

libraryDependencies += "mysql" % "mysql-connector-java" % "8.0.13" % Runtime

libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.25"
libraryDependencies += "log4j" % "log4j" % "1.2.17"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.1"


//addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")





