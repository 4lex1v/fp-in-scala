name := "FP in Scala"

scalaVersion := "2.10.2"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.4",
  "org.scalaz" %% "scalaz-effect" % "7.0.4",
  "org.scalaz" %% "scalaz-concurrent" % "7.0.4",
  "org.scalaz.stream" %% "scalaz-stream" % "0.2-SNAPSHOT",
  "com.spinoco" %% "scalaz-stream-mongodb" % "0.1.0"
)