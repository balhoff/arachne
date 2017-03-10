
organization  := "org.geneontology"

name          := "arachne"

version       := "0.0.3"

publishMavenStyle := true

publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
        Some("snapshots" at nexus + "content/repositories/snapshots")
    else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

homepage := Some(url("https://github.com/balhoff/arachne"))

scalaVersion  := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

javaOptions += "-Xmx10G"

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.scalaz"                  %% "scalaz-core"            % "7.2.1",
    "org.apache.jena"             %  "apache-jena-libs"       % "3.2.0" pomOnly(),
    "com.typesafe.scala-logging"  %% "scala-logging"          % "3.4.0",
    "ch.qos.logback"              %  "logback-classic"        % "1.1.7" % Test,
    "org.codehaus.groovy"         %  "groovy-all"             % "2.4.6" % Test,
    "org.geneontology"            %% "owl-to-rules"           % "0.2"   % Test,
    "org.scalatest"               %% "scalatest"              % "3.0.1" % Test
  )
}

pomExtra := (
    <scm>
        <url>git@github.com:balhoff/arachne.git</url>
        <connection>scm:git:git@github.com:balhoff/arachne.git</connection>
    </scm>
    <developers>
        <developer>
            <id>balhoff</id>
            <name>Jim Balhoff</name>
            <email>jim@balhoff.org</email>
        </developer>
    </developers>
)
