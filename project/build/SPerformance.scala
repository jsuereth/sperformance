import sbt._

class SPerformanceProject(info: ProjectInfo) extends DefaultProject(info)
{
  lazy val hi = task { println("Hello World"); None }

  val jfreechart = "jfree" % "jfreechart" % "1.0.12"
  val sbt_test_interface = "org.scala-tools.testing" % "test-interface" % "0.4"
}
