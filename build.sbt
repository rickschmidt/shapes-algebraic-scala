name := "shapes-algebraic-scala"

version := "1.0"

scalaVersion := "2.9.1"

// set the main Scala source directory to be <base>/src
scalaSource in Compile <<= baseDirectory(_ / "src")

// set the Scala test source directory to be <base>/test
scalaSource in Test <<= baseDirectory(_ / "test")
override def testScalaSourcePath = "test/edu/luc/cs/laufer/cs473/shapealgebra/"