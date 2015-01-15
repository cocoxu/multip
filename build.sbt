name := "multip"

mainClass in (Compile, run) := Some("multip.Main")

version := "0.1"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies  ++= Seq(
            // other dependencies here
            "org.scalala" %% "scalala" % "1.0.0.RC3-SNAPSHOT",
	    "org.clapper" %% "argot" % "0.4",
	    "org.scalacheck" %% "scalacheck" % "1.9" % "test",
	    "org.apache.commons" % "commons-math" % "2.2"
)

resolvers ++= Seq(
            // other resolvers here
            "Scala Tools Snapshots" at "https://oss.sonatype.org/content/groups/scala-tools/repo-snapshots/",
            "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
)

scalaVersion := "2.9.2"
