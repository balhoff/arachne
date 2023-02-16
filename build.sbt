enablePlugins(JavaAppPackaging)

organization  := "org.geneontology"

name          := "arachne"

version       := "1.3"

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

scalaVersion  := "2.13.10"

//crossScalaVersions := Seq("2.12.15", "2.13.6")

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

mainClass in Compile := Some("org.geneontology.rules.cli.Main")

javaOptions += "-Xmx10G"

fork in Test := true

libraryDependencies ++= {
  Seq(
    "org.scalaz"                  %% "scalaz-core"            % "7.3.7",
    "org.apache.jena"             %  "apache-jena-libs"       % "3.17.0" pomOnly(),
    "org.geneontology"            %% "owl-to-rules"           % "0.3.8",
    "net.sourceforge.owlapi"      %  "owlapi-distribution"    % "4.5.25",
    "org.backuity.clist"          %% "clist-core"             % "3.5.1",
    "org.backuity.clist"          %% "clist-macros"           % "3.5.1" % "provided",
    "com.outr"                    %% "scribe-slf4j"           % "2.8.3",
    "org.scalatest"               %% "scalatest"              % "3.2.14" % Test
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
