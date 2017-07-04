enablePlugins(DockerPlugin)

name := "simple-publisher"

version := "1.0"

scalaVersion := "2.12.1"

resolvers += "MQTT Repository" at "https://repo.eclipse.org/content/repositories/paho-releases/"

libraryDependencies += "org.eclipse.paho" % "mqtt-client" % "0.4.0"
libraryDependencies += "org.scodec" % "scodec-core_2.12" % "1.10.3"

dockerfile in docker := {

  val artifact: File = assembly.value
  val artifactTargetPath = s"/app/${artifact.name}"

  new Dockerfile {
    from("java")
    add(artifact,artifactTargetPath)
    entryPoint("java", "-jar", artifactTargetPath)
  }
}
