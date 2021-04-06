name := "SoundCam"

version := "1.0"

scalaVersion := "2.13.5"

val scala2Version = "2.13.5"
val scala3Version = "3.0.0-RC1"
// To cross compile with Scala 2 and Scala 3
crossScalaVersions := Seq(scala2Version, scala3Version)
scalaVersion := scala2Version

lazy val akkaVersion = "2.6.13"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "org.scala-lang.modules" %% "scala-xml" % "1.3.0",
  "org.scalafx" %% "scalafx-extras" % "0.3.6",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.2.6" % Test
)

// https://mvnrepository.com/artifact/com.typesafe.akka/akka-stream
// libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaVersion

// https://mvnrepository.com/artifact/org.scalafx/scalafx
libraryDependencies += "org.scalafx" %% "scalafx" % "15.0.1-R21"

// Add JavaFX dependencies
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux") => "linux"
  case n if n.startsWith("Mac") => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

libraryDependencies ++= javaFXModules.map(m =>
  "org.openjfx" % s"javafx-$m" % "15.0.1" classifier osName
)

resolvers += Opts.resolver.sonatypeSnapshots

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint")

// Sources should also be copied to output, so the sample code, for the viewer,
// can be loaded from the same file that is used to execute the example
unmanagedResourceDirectories in Compile += baseDirectory(_ / "src/main/scala").value

// Set the prompt (for this build) to include the project id.
shellPrompt := { state => System.getProperty("user.name") + ":" + Project.extract(state).currentRef.project + "> " }

// Run in separate VM, so there are no issues with double initialization of JavaFX
fork := true

fork in Test := true

// Create file used to determine available examples at runtime.
resourceGenerators in Compile += Def.task {
  /** Scan source directory for available examples
   * Return pairs 'directory' -> 'collection of examples in that directory'.
   */
  def loadExampleNames(inSourceDir: File): Array[(String, Array[String])] = {
    val examplesDir = "/scalafx/ensemble/example/"
    val examplePath = new File(inSourceDir, examplesDir)
    val exampleRootFiles = examplePath.listFiles()
    for (dir <- exampleRootFiles if dir.isDirectory) yield {
      val leaves = for (f <- dir.listFiles() if f.getName.contains(".scala")) yield {
        f.getName.stripSuffix(".scala").stripPrefix("Ensemble")
      }
      dir.getName.capitalize -> leaves.sorted
    }
  }

  /** Create file representing names and directories for all availabe examples.
   * It will be loaded by the application at runtime and used to popolate example tree.
   */
  def generateExampleTreeFile(inSourceDir: File,
                              outSourceDir: File,
                              templatePath: String): Seq[File] = {
    val exampleDirs = loadExampleNames(inSourceDir)
    val contents = exampleDirs.map { case (dir, leaves) => dir + " -> " + leaves.mkString(", ") }.mkString("\n")
    val outFile = new File(outSourceDir, templatePath)
    IO.write(outFile, contents)

    Seq(outFile)
  }

  generateExampleTreeFile(
    (scalaSource in Compile).value,
    (resourceManaged in Compile).value,
    "/scalafx/ensemble/example/example.tree"
  )
}.taskValue

cleanFiles := (baseDirectory.value / "application.log") +: cleanFiles.value

assemblyMergeStrategy in assembly := {
  case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
  case "application.conf"                            => MergeStrategy.concat
  case "module-info.class"                                => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

//unmanagedJars in Runtime ++= Seq(new java.io.File(".")).classpath

mainClass in Compile := Some("com.apaa.SoundCamApp")
javaOptions in run += "-Djava.library.path=lib"

