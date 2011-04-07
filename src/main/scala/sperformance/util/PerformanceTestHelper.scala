package sperformance
package util


/**t
 * Helper methods for running performance tests.
 */
private[sperformance] object PerformanceTestHelper {

  /** Currently the number of times we run the warm up test when warming up the JVM. */
  lazy val warmUpRuns = 100000 //TODO - Lookup in some manenr...

  /**
   * Aggregates test results by taking an average.
   */
  val AverageAggregator = { results : Seq[Long] =>
    results.sum / results.size
  }

  /**
   * Aggregates test results by taking the minimum value
   */
  val MinAggregator = { results : Seq[Long] =>
    results.min
  }

  /**
   * Aggregates test reuslts by taking the maximum value
   */
  val MaxAggregator = { results : Seq[Long] =>
    results.max
  }

  /**
   * Aggregates test results by taking the mediam value
   */
  val MedianAggregator = { results : Seq[Long] =>
    results.sorted.apply(results.size/2)
  }

  /**
   * Aggregates test results by taking the man of values withing two standard deviations of the original mean.
   */
  val MeanWithoutOutliers = { results : Seq[Long] =>
    val firstAvg = AverageAggregator(results)
    val deviation = AverageAggregator(results.map( result => math.abs(result - firstAvg)))
    val dev2 = deviation*2
    AverageAggregator(results.filter( result => math.abs(result - firstAvg) < dev2))
  }

  /** Runs a given function enough that the HotSpot JVM should optimize the method.
   * TODO - Do not warm up non JIT/HotSpot JVMs.
   */
  def warmUpJvm(method: Function0[Unit]) {
     for(i <- 1 to warmUpRuns) method.apply()
   }

  /**
   * Measures the execution of a function.
   * @param method
   *           The method being measured.
   * @return
   *      The nanosecond execution time of the function.
   */
  def measureOnce(method : Function0[Unit]) : Long = {
    val startNano = System.nanoTime
    method.apply()
    val endNano = System.nanoTime
    endNano - startNano
  }

  /**
   * Measures the execution time of a method several times, and returns the results
   * @param runs
   *          The number of times to run the method
   * @param method
   *          The method being measured
   * @return
   *      Sequence of execution times for a method.
   */
  def measureMultiple(runs: Int)(method: Function0[Unit]) = for(i <- 1 to runs) yield measureOnce(method)

  /**
   * Measures the execution time of a method and aggregates the reuslts into one number using a given aggregator.
   * @param method
   *               The method under test
   * @param combineResults
   *                The mechanism of aggregating results (defaults to Minimum value)
   * @return
   *                The aggregated result of the  performance test.
   */
  def measure(method : Function0[Unit])(implicit combineResults : Seq[Long] => Long = MinAggregator) : Long = {
    combineResults(measureMultiple(10)(method))
  }



}