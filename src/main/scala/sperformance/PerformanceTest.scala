package sperformance

import generators._

/**
 * @param time  The average time it took to run the test
 * @param axisData  Data relating to alternative axes, such as size of collections.  For checking algorithmic growth
 * @param attributes  Random attribtues relating to this run e.g. the class/method under test.
 */
case class PerformanceTestResult(time : Long, axisData : Map[String, Any], attributes : Map[String, Any]) {
}

/** A Handler for specific performance test results */
trait PerformanceTestRunContext { self =>
  def reportResult(result : PerformanceTestResult) : Unit

  /**
   * Creates a new PerformanceTestRunContext that will append the given attribute to tests.
   */
  def addAttribute[T,U](attr : (String,U)) : PerformanceTestRunContext = new DelegatedPerformanceTestRunContext(self) {
    override def reportResult(result : PerformanceTestResult) = super.reportResult(result.copy(attributes = result.attributes + attr))
    override def toString : String = "AtrributeDelegatingCtx(" + attr + " to " + self + ")"
  }
  /**
   * Creates a new PerformanceTestRunContext that will append the given attribute to tests.
   */
  def addAxisValue[T,U](axisVal : (String,U)) : PerformanceTestRunContext = new DelegatedPerformanceTestRunContext(self) {
    override  def reportResult(result : PerformanceTestResult) = super.reportResult(result.copy(axisData = result.axisData + axisVal))
    override def toString : String = "AxiseDelegatingCtx(" + axisVal + " to " + self + ")"
  }
}

/**
 * Delegating version of the PerformanceTestRunContext
 */
abstract class DelegatedPerformanceTestRunContext(delegate : PerformanceTestRunContext) extends PerformanceTestRunContext {  
  override def reportResult(result : PerformanceTestResult) : Unit = delegate.reportResult(result)
}

object NullPerformanceTestRunContext extends PerformanceTestRunContext {
  override def reportResult(result : PerformanceTestResult) : Unit = {}
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


  /**
   * This method will execute a performance test.
   */
  def runTest(context :PerformanceTestRunContext) : Unit  = {  }


}

