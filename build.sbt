import BuildHelper._

inThisBuild(
  List(
    organization := "dev.zio",
    homepage := Some(url("https://github.com/zio/zio-codec/")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer("mijicd", "Dejan Mijic", "dmijic@acm.org", url("https://github.com/mijicd")),
      Developer(
        "sergei-shabanau",
        "Sergei Shabanau",
        "serge.shabanau@gmail.com",
        url("https://github.com/sergei-shabanau")
      ),
      Developer("regis-leray", "Regis Leray", "regis.leray@gmail.com", url("https://github.com/regis-leray")),
      Developer("soujiro32167", "Eli Kasik", "soujiro32167@gmail.com", url("https://github.com/soujiro32167")),
      Developer(
        "jczuchnowski",
        "Jakub Czuchnowski",
        "jakub.czuchnowski@gmail.com",
        url("https://github.com/jczuchnowski")
      )
    ),
    pgpPassphrase := sys.env.get("PGP_PASSWORD").map(_.toArray),
    pgpPublicRing := file("/tmp/public.asc"),
    pgpSecretRing := file("/tmp/secret.asc"),
    scmInfo := Some(
      ScmInfo(url("https://github.com/zio/zio-codec/"), "scm:git:git@github.com:zio/zio-codec.git")
    )
  )
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("check", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")

lazy val codec =
  project
    .in(file("."))
    .settings(stdSettings("zio-codec"))
    .settings(buildInfoSettings("zio.codec"))
    .settings(
      libraryDependencies ++= Seq(
        "org.ow2.asm" % "asm"           % "8.0.1",
        "dev.zio"     %% "zio"          % zioVersion,
        "dev.zio"     %% "zio-test"     % zioVersion % Test,
        "dev.zio"     %% "zio-test-sbt" % zioVersion % Test
      ),
      fork in Test := true,
      parallelExecution in Test := false,
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
    )
    .enablePlugins(BuildInfoPlugin)
