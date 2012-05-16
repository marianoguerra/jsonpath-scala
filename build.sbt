name := "jsonpath-scala"

version := "0.1"                                                       

organization := "marianoguerra"                                        

scalaVersion := "2.9.1"   

scalacOptions ++= Seq("-unchecked", "-deprecation")

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

resolvers += "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

resolvers += "Maven2 Repository" at "http://repo2.maven.org/maven2/"

resolvers += "Apache Maven2 Repository" at "http://repo1.maven.org/maven2/"

libraryDependencies ++= Seq(
    "net.liftweb" %% "lift-json" % "2.4" % "compile",
    "org.scalatest" % "scalatest_2.9.0" % "1.7.2" % "test"
)   
