package sperformance
package util



private[sperformance] object PerformanceTestHelper {

  lazy val warmUpRuns = 10000 //TODO - Lookup in some manenr...

  val AverageAggregator = { results : Seq[Long] =>
    results.sum / results.size
  }

  val MinAggregator = { results : Seq[Long] =>
    results.min
  }

  val MaxAggregator = { results : Seq[Long] =>
    results.max
  }

  val MedianAggregator = { results : Seq[Long] =>
    results.sorted.apply(results.size/2)
  }

  val MeanWithoutOutliers = { results : Seq[Long] =>
    val firstAvg = AverageAggregator(results)
    val deviation = AverageAggregator(results.map( result => math.abs(result - firstAvg)))
    val dev2 = deviation*2
    AverageAggregator(results.filter( result => math.abs(result - firstAvg) < dev2))
  }


  def warmUpJvm(method: Function0[Unit]) {
     for(i <- 1 to warmUpRuns) method.apply()
   }


  def measureOnce(method : Function0[Unit]) : Long = {
    val startNano = System.nanoTime
    method.apply()
    val endNano = System.nanoTime
    endNano - startNano
  }

  def measureMultiple(runs: Int)(method: Function0[Unit]) = for(i <- 1 to runs) yield measureOnce(method)

  def measure(method : Function0[Unit])(implicit combineResults : Seq[Long] => Long = MinAggregator) : Long = {
    combineResults(measureMultiple(5)(method))
  }



}