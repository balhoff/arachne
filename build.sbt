
organization  := "org.geneontology"

name          := "rule-engine"

version       := "0.0.1-SNAPSHOT"

publishArtifact in Test := false

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

scalaVersion  := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

scalacOptions in Test ++= Seq("-Yrangepos")

javaOptions in Test += "-Xmx10G"

fork in Test := true

libraryDependencies ++= {
  Seq(
    "net.sourceforge.owlapi"      %  "owlapi-distribution"    % "4.2.7",
    "org.apache.jena"             %  "apache-jena-libs"       % "3.2.0" pomOnly(),
    "org.phenoscape"              %% "scowl"                  % "1.2.1",
    "com.typesafe.akka"           %% "akka-actor"             % "2.4.17",
    "com.typesafe.scala-logging"  %% "scala-logging"          % "3.4.0",
    "ch.qos.logback"              %  "logback-classic"        % "1.1.7",
    "org.codehaus.groovy"         %  "groovy-all"             % "2.4.6",
    "org.geneontology"            %% "owl-to-rules"           % "0.0.1",
    "org.scalaz"                  %% "scalaz-core"            % "7.2.1",
    "net.sourceforge.owlapi"      %  "org.semanticweb.hermit" % "1.3.8.413" % Test,
    "org.scalatest"               %% "scalatest"              % "3.0.1" % Test
  )
}

