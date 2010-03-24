 package sperformance
 package generators


import util.PerformanceTestHelper

/** Generator that iterates through sizes and generates performance results */
class SizeGenerator[T](startSize : Int, endSize : Int, module : String, method : String, setup : Int => T) {
  private[sperformance] lazy val medianSize = (endSize - startSize) / 2


  def run(f : T => Unit)(implicit handler : ReportHandler) : Unit = {
    import PerformanceTestHelper._
     //Warmup JVM
     {
       val warmUpState = setup(medianSize)
       warmUpJvm(() => f(warmUpState))  //TODO - Warmup on median ok?
     }
     val result = for {
       size <- startSize to endSize
       val setupState = setup(size)
       val time = measure(() => f(setupState))
     } handler.reportResult(PerformanceTestResult(time,
         Map("size" -> size),
         Map("module" -> module, "method" -> method)
      ))

  }
}