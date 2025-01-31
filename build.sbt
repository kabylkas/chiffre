def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

organization := "com.ibm"
name := "chiffre"
version := "0.1-SNAPSHOT"
scalacOptions := Seq("-deprecation", "-feature") ++ scalacOptionsVersion(scalaVersion.value)
scalaVersion := "2.12.8"
crossScalaVersions := Seq("2.11.12", "2.12.4")
/* Assembly  
libraryDependencies += "com.github.scopt" %% "scopt" % "3.6.0"
libraryDependencies += "org.json4s" %% "json4s-native" % "3.5.3"

assemblyJarName in assembly := "chiffre.jar"
test in assembly := {}
assemblyOutputPath in assembly := file("./utils/bin/chiffre.jar")

val defaultVersions = Map("chisel3" -> "3.1.8",
                          "chisel-iotesters" -> "1.2.+",
                          "firrtl"  -> "1.1.+" )

libraryDependencies ++= defaultVersions.map{ case (k, v) =>
  "edu.berkeley.cs" %% k % sys.props.getOrElse(k + "Version", v) }.toSeq */
