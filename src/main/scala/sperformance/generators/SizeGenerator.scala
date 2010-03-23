 package sperformance
 package generators


 import util.PerformanceTestHelper

/** Generator that iterates through sizes and generates performance results */
class SizeGenerator(startSize : Int, endSize : Int, override val module : String, override val method : String) extends Generator[Int] {
  private[sperformance] lazy val medianSize = (endSize - startSize) / 2

  def run(f : Int => Unit)(implicit handler : ReportHandler) : Unit = {
    import PerformanceTestHelper._
     warmUpJvm(() => f(medianSize))  //TODO - Warmup on median ok?
     val result = for {
       size <- startSize to endSize
       val time = measure(() => f(size))
     } handler.reportResult(PerformanceTestResult(time,
         Map("size" -> size),
         Map("module" -> module, "method" -> method)
      ))

  }
}