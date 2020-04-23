name := "markovNamegen"

version := "0.1"

scalaVersion := "2.13.1"

val zioVersion = "1.0.0-RC18-2"
libraryDependencies ++= Seq(
  "dev.zio"                 %% "zio"             % zioVersion,
  "dev.zio"                 %% "zio-test"        % zioVersion % "test",
  "dev.zio"                 %% "zio-test-sbt"    % zioVersion % "test",
  "com.github.BraianIDeRoo" % "random-util_2.13" % "0.1"
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
