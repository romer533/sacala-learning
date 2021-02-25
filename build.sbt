name := "scala-learning"

version := "0.1"

scalaVersion := "2.13.4"

val AkkaVersion = "2.6.12"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % AkkaVersion % Test
)

val logbackVersion      = "1.2.3"
val scalaLoggingVersion = "3.9.2"

val logback      = "ch.qos.logback"              % "logback-classic" % logbackVersion
val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging"   % scalaLoggingVersion
