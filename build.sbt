
lazy val root = (project in file(".")).
  settings(
    organization := "cddamod",
    name := "cdda-modding-helper",
    version := "1.3.0",
    scalaVersion := "2.11.8",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.+",
      "org.scalaz" %% "scalaz-effect" % "7.2.+",
      "org.json4s" %% "json4s-native" % "3.5.+",
      "org.json4s" %% "json4s-scalaz" % "3.5.+"
    ),
    scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation", "-Xlint",
			  "-Ywarn-dead-code", "-Ywarn-unused", "-Ywarn-unused-import"),
    mainClass in assembly := Some("cddamod.Main"),
    test in assembly := {}
  )
