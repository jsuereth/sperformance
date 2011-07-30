package sperformance

import generators._

/**
 * This class represents all state recorded from a given performance test.  It is the quantum of communication of data in
 * the entire system.
 *
 * @param time  The average time it took to run the test
 * @param axisData  Data relating to alternative axes, such as size of collections.  For checking algorithmic growth
 * @param attributes  Random attributes relating to this run e.g. the class/method under test.
 */
case class PerformanceTestResult(time: Long, axisData: Map[String, Any], attributes: Map[String, Any])

/**
 * A Handler for specific performance test results.   This is the means by which generators report testing
 * results.   This can be modified with various attributes/axis values before being passed to a generator of tests.
 * Also, it allows users to define their own mechanisms of testing performance and reporting results.
 */
trait PerformanceTestRunContext { self =>

  def attribute[U](key: String): Option[U]
  def axisValue[U](key: String): Option[U]
  /** Reports a test result to this context.  The current attributes/axis values are appended and sent to the performance reporting engine. */
  def reportResult(result: PerformanceTestResult): Unit

  /**
   * Creates a new PerformanceTestRunContext that will append the given attribute to tests.
   */
  def addAttribute[T, U](attr: (String, U)): PerformanceTestRunContext = new DelegatedPerformanceTestRunContext(self) {
    override def reportResult(result: PerformanceTestResult) = super.reportResult(result.copy(attributes = result.attributes + attr))
    override def attribute[U](key: String): Option[U] = if (attr._1 == key) Some(attr._2.asInstanceOf[U]) else self.attribute[U](key)
    override def toString: String = "AtrributeDelegatingCtx(" + attr + " to " + self + ")"
  }
  /**
   * Creates a new PerformanceTestRunContext that will append the given attribute to tests.
   */
  def addAxisValue[T, U](axisVal: (String, U)): PerformanceTestRunContext = new DelegatedPerformanceTestRunContext(self) {
    override def reportResult(result: PerformanceTestResult) = super.reportResult(result.copy(axisData = result.axisData + axisVal))
    override def axisValue[U](key: String): Option[U] = if (axisVal._1 == key) Some(axisVal._2.asInstanceOf[U]) else self.attribute[U](key)
    override def toString: String = "AxiseDelegatingCtx(" + axisVal + " to " + self + ")"
  }
}

/**
 * Delegating version of the PerformanceTestRunContext.   This class is used to make the decorator pattern (used in
 * the addAttribute and addAxisValue methods) easier to implement.
 */
abstract class DelegatedPerformanceTestRunContext(delegate: PerformanceTestRunContext) extends PerformanceTestRunContext {
  override def reportResult(result: PerformanceTestResult): Unit = delegate.reportResult(result)
  override def attribute[U](key: String): Option[U] = delegate.attribute[U](key)
  override def axisValue[U](key: String): Option[U] = delegate.axisValue[U](key)
}

object NullPerformanceTestRunContext extends PerformanceTestRunContext {
  override def reportResult(result: PerformanceTestResult): Unit = {}
  override def attribute[U](key: String): Option[U] = None
  override def axisValue[U](key: String): Option[U] = None

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
  def name: String = {
    val className = getClass.getCanonicalName

    def isObject = className.endsWith("$")
    def isTrait = className.endsWith("$class")
    def isClass = !isObject && !isTrait

    if (isObject) "Object-" + className.dropRight(1) else "Class-" + className
  }

  /**
   * This method will execute a performance test.
   */
  def runTest(context: RunContext): Unit = { context.testFinished(this) }

}

