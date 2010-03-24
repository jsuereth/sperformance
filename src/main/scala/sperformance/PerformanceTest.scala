package sperformance

import generators._

/**
 * @param time  The average time it took to run the test
 * @param axisData  Data relating to alternative axes, such as size of collections.  For checking algorithmic growth
 * @param attributes  Random attribtues relating to this run e.g. the class/method under test.
 */
case class PerformanceTestResult(time : Long, axisData : Map[String, Any], attributes : Map[String, Any])

/** A Handler for specific performance test results */
trait ReportHandler {
  def reportResult(result : PerformanceTestResult) : Unit  
}







/**
 * This trait is mixed in when you want to define performance testing runs
 */
trait PerformanceTest {

  /**
   * Returns the name of the test being run...
   *
   * Default implementation is to look up reflectively the name of the class...
   */
  def name : String = {
    val className = getClass.getCanonicalName

    def isObject = className.endsWith("$")
    def isTrait = className.endsWith("$class")
    def isClass = !isObject && !isTrait

    if(isObject) "Object-" + className.dropRight(1) else "Class-" + className
  }

  //Default handler for now...
  implicit def handler : ReportHandler = new ReportHandler {
    def reportResult(result : PerformanceTestResult) {
        Console.println(result)
    }
  }


  /**
   * This method will execute a performance test.
   */
  def runTest(context :RunContext) : Unit  = {  }


}

