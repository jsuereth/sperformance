package sperformance

import generators._

case class PerformanceTestResult(time : Long, axisData : Map[String, Any], attributes : Map[String, Any])

/** A Handler for reports*/
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
  def runTest() : Unit  = {  }

  
}

