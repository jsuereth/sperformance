package sperformance
package util

private[sperformance] object PerformanceTestHelper {

  lazy val warmUpRuns = 10000 //TODO - Lookup in some manenr...

  def warmUpJvm(method: Function0[Unit]) {
     for(i <- 1 to warmUpRuns) method.apply()
   }


  def measureOnce(method : Function0[Unit]) : Long = {
    val startNano = System.nanoTime
    method.apply()
    val endNano = System.nanoTime
    endNano - startNano
  }
  def measureAvg(runs : Int)(method : Function0[Unit]) : Long = {
    val results = for(i <- 1 to runs) yield measureOnce(method)
    results.sum / results.size
  }

  def measure(method : Function0[Unit]) : Long = {
    measureAvg(5)(method)
  }
}