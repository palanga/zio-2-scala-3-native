scalaVersion := "3.2.0"

name := "scala-3-native-zio-2"

enablePlugins(ScalaNativePlugin)

libraryDependencies += "dev.zio" %%% "zio" % "2.0.2"

// This is needed for the linking step
libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.4.0"

scalacOptions +=  "-source:future"
