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

ThisBuild / scalaVersion := "2.13.2"

skip in publish := true

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

val zioVersion = "1.0.0-RC20"

val markovNamegen = crossProject(JSPlatform, JVMPlatform)
  .in(file("."))
  .settings(
    name := "markovNamegen",
    version := "0.5.2",
    libraryDependencies ++= Seq(
      "dev.zio"                 %%% "zio"          % zioVersion,
      "dev.zio"                 %%% "zio-test"     % zioVersion % "test",
      "dev.zio"                 %%% "zio-test-sbt" % zioVersion % "test",
      "com.github.BraianIDeRoo" %%% "random-util"  % "0.5.2"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    publishTo := Some(
      if (isSnapshot.value)
        Opts.resolver.sonatypeSnapshots
      else
        Opts.resolver.sonatypeStaging
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.0.0"
  )

lazy val root = project
  .in(file("."))
  .aggregate(markovNamegen.js, markovNamegen.jvm)
  .settings(scalaVersion := "2.13.2", publish := {}, publishLocal := {})
