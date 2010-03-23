package sperformance

import testing.SUnit.Test

case class PerformanceTestResult(time : Long, attributes : Map[String, Any])

/** A Handler for reports*/
trait ReportHandler {
  def reportResult(result : PerformanceTestResult) : Unit  
}

/** Interface for generators */
trait Generator[T] {
  val module : String
  val method : String  
  def run(f : T => Unit)(implicit handler : ReportHandler) : Unit
}


private[sperformance] object PerformanceTestHelper {

  lazy val warmUpRuns = 10000 //TODO - Lookup in some manenr...

  def warmUpJvm(method: Function0[Unit]) {
     for(i <- 1 to warmUpRuns) method.apply()
   }


  def measure(method : Function0[Unit]) : Long = {
    val startNano = System.nanoTime
    method.apply()
    val endNano = System.nanoTime
    endNano - startNano
  }

}

/** Generator that iterates through sizes and generates performance results */
class SizeGenerator(startSize : Int, endSize : Int, override val module : String, override val method : String) extends Generator[Int] {
  private[sperformance] lazy val medianSize = (endSize - startSize) / 2

  def run(f : Int => Unit)(implicit handler : ReportHandler) : Unit = {
    import PerformanceTestHelper._
     warmUpJvm(() => f(medianSize))  //TODO - Warmup on median ok?
     val result = for {
       size <- startSize to endSize
       val time = measure(() => f(size))
     } handler.reportResult(new PerformanceTestResult(time,
         Map("size" -> size, "module" -> module, "method" -> method)))

  }
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

