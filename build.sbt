enablePlugins(JavaAppPackaging)

organization  := "org.geneontology"

name          := "arachne"

version       := "0.0.4-SNAPSHOT"

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

mainClass in Compile := Some("org.geneontology.rules.cli.Main")

javaOptions += "-Xmx10G"

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.scalaz"                  %% "scalaz-core"            % "7.2.1",
    "org.apache.jena"             %  "apache-jena-libs"       % "3.2.0" pomOnly(),
    "org.geneontology"            %% "owl-to-rules"           % "0.2",
    "net.sourceforge.owlapi"      %  "owlapi-distribution"    % "4.2.8",
    "org.backuity.clist"          %% "clist-core"             % "3.2.2",
    "org.backuity.clist"          %% "clist-macros"           % "3.2.2" % "provided",
    "com.typesafe.scala-logging"  %% "scala-logging"          % "3.4.0",
    "ch.qos.logback"              %  "logback-classic"        % "1.1.7" % Test,
    "org.codehaus.groovy"         %  "groovy-all"             % "2.4.6" % Test,
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
