name := "markovNamegen"

inThisBuild(
  List(
    organization := "com.github.BraianIDeRoo",
    homepage := Some(url("https://github.com/BraianIDeRoo/scala-markov-namegen")),
    licenses := List(
      "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")
    ),
    developers := List(
      Developer(
        "BraianIDeRoo",
        "Braian De Roo",
        "braianideroo@gmail.com",
        url("https://github.com/BraianIDeRoo")
      )
    ),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/BraianIDeRoo/scala-markov-namegen"),
        "git@github.com:BraianIDeRoo/scala-markov-namegen.git"
      )
    )
  )
)

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

scalaVersion := "2.13.2"

val zioVersion = "1.0.0-RC19-2"
libraryDependencies ++= Seq(
  "dev.zio"                 %% "zio"             % zioVersion,
  "dev.zio"                 %% "zio-test"        % zioVersion % "test",
  "dev.zio"                 %% "zio-test-sbt"    % zioVersion % "test",
  "com.github.BraianIDeRoo" % "random-util_2.13" % "0.5.0"
)
testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
