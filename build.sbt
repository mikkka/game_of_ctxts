ThisBuild / scalaVersion     := "2.12.15"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"
ThisBuild / semanticdbEnabled := true

// scalacOptions += "-Ymacro-annotations"

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full) 
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
libraryDependencies += "org.typelevel" %% "cats-mtl" % "1.2.0"
libraryDependencies += "org.typelevel" %% "cats-tagless-macros" % "0.14.0"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.3.1"
libraryDependencies += "dev.zio" %% "zio" % "1.0.12"
