name := "Nozomi"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies  ++= Seq(
    // other dependencies here
    "org.scalanlp" %% "breeze" % "0.12",
    "org.scalanlp" %% "breeze-natives" % "0.12",
    "org.scalanlp" %% "breeze-viz" % "0.12",
    "org.scalactic" %% "scalactic" % "3.0.1",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "ch.qos.logback" % "logback-classic" % "1.1.5",
    "org.json4s" % "json4s-native_2.11" % "3.3.0"
)

resolvers ++= Seq(
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
    "Artima Maven Repository" at "http://repo.artima.com/releases"
)