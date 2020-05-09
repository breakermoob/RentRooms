name := """RentRooms"""
organization := "co.edu.udea"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala).disablePlugins(PlayFilters)

scalaVersion := "2.12.3"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % Test
// Estas son las dependencias necesarias para trabajar con bases de datos MySQL
libraryDependencies += jdbc
libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.41"//"8.0.20"

libraryDependencies += filters

// Adds additional packages into Twirl
//TwirlKeys.templateImports += "co.edu.udea.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "co.edu.udea.binders._"
