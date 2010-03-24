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

