import net.virtualvoid.sbt.graph.Plugin
import org.scalastyle.sbt.ScalastylePlugin
import NewmanReleaseSteps._
import sbtrelease._
import ReleaseStateTransformations._
import ReleasePlugin._
import ReleaseKeys._
import sbt._

name := "newman"

organization := "com.stackmob"

scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.0", "2.10.1")

scalacOptions <++= (scalaVersion).map { version: String =>
  val defaults = Seq("-unchecked", "-deprecation")
  if (version.startsWith("2.10")) {
    defaults ++ Seq("-feature", "-language:implicitConversions,", "-language:higherKinds")
  } else {
    defaults
  }
}

libraryDependencies ++= {
  val httpCoreVersion = "4.2.1"
  val httpClientVersion = "4.2.1"
  val scalaCheckVersion = "1.10.0"
  val specs2Version = "1.12.3"
  val mockitoVersion = "1.9.0"
  val scalazVersion = "6.0.4"
  val liftJsonVersion = "2.5-RC2"
  Seq(
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "org.apache.httpcomponents" % "httpcore" % httpCoreVersion,
    "org.apache.httpcomponents" % "httpclient" % httpClientVersion,
    "net.liftweb" %% "lift-json-scalaz" % liftJsonVersion,
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test",
    "org.specs2" %% "specs2" % specs2Version % "test",
    "org.pegdown" % "pegdown" % "1.0.2" % "test",
    "org.mockito" % "mockito-all" % mockitoVersion % "test"
  )
}

logBuffered := false

ScalastylePlugin.Settings

Plugin.graphSettings

releaseSettings

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  setReadmeReleaseVersion,
  tagRelease,
  publishArtifacts,
  setNextVersion,
  commitNextVersion,
  pushChanges
)

publishTo <<= (version) { version: String =>
  val nexus = "https://oss.sonatype.org/"
  if (version.trim.endsWith("SNAPSHOT")) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
   } else {
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}

publishMavenStyle := true

publishArtifact in Test := true

testOptions in Test += Tests.Argument("html", "console")

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/stackmob/newman</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:stackmob/newman.git</url>
    <connection>scm:git:git@github.com:stackmob/newman.git</connection>
  </scm>
  <developers>
    <developer>
      <id>arschles</id>
      <name>Aaron Schlesinger</name>
      <url>http://www.stackmob.com</url>
    </developer>
    <developer>
      <id>devmage</id>
      <name>Andrew Harris</name>
      <url>http://www.stackmob.com</url>
    </developer>
    <developer>
      <id>taylorleese</id>
      <name>Taylor Leese</name>
      <url>http://www.stackmob.com</url>
    </developer>
    <developer>
      <id>kelseyq</id>
      <name>Kelsey Innis</name>
      <url>http://www.stackmob.com</url>
    </developer>
    <developer>
      <id>jrwest</id>
      <name>Jordan West</name>
      <url>http://github.com/jrwest</url>
    </developer>
  </developers>
)
