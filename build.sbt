name := "markovNamegen"

version := "0.1"

scalaVersion := "2.13.1"

val zioVersion = "1.0.0-RC18-1"
libraryDependencies ++= Seq(
  "dev.zio" %% "zio"          % zioVersion,
  "dev.zio" %% "zio-test"     % zioVersion % "test",
  "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
