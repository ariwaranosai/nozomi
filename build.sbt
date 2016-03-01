name := "Nozomi"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies  ++= Seq(
    // other dependencies here
    "org.scalanlp" %% "breeze" % "0.12",
    "org.scalanlp" %% "breeze-natives" % "0.12",
    "org.scalanlp" %% "breeze-viz" % "0.12",
    "org.scalactic" %% "scalactic" % "2.2.6",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"
)

resolvers ++= Seq(
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
    "Artima Maven Repository" at "http://repo.artima.com/releases"
)