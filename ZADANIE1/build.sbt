scalaVersion := "2.13.14"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "cask"    % "0.10.2",
  "com.lihaoyi" %% "upickle" % "4.0.2"
)


Compile / run / mainClass := Some("app.Zadanie1")
