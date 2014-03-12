name := "FP in Scala"

scalaVersion := "2.10.3"

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

libraryDependencies ++= Seq(
  "org.scalaz"        %% "scalaz-core" % "7.0.4",
  "org.scalaz"        %% "scalaz-effect" % "7.0.4",
  "org.scalaz"        %% "scalaz-concurrent" % "7.0.4",
  "org.scalaz.stream" %% "scalaz-stream" % "0.3.1",
  "com.spinoco"       %% "scalaz-stream-mongodb" % "0.1.0"
)