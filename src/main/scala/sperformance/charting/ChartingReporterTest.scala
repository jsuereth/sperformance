package sperformance
package charting

import intelligence._

/**
 * This trait can be mixed in if you wish to generate charts after running your performance benchmarks.
 */
trait ChartingReporterTest extends PerformanceTest with ClusteringTest {

  //Override runTest so it generates reports when finished
  override def runTest(context: RunContext) : Unit  = {
    super.runTest(context)
    Charting.createReports(context)
  }

  //hide all our variables in this object
  object Charting {
    val chartGenerators : Seq[ChartGenerator] = SizeChartGenerator :: Nil

    def createReports(context : RunContext) {
      //Find data...
      for {
        (_, cluster) <- clusters
        gen <- chartGenerators
        if(gen.canHandleCluster(cluster))
      } {
        gen.generateChart(cluster, context)
      }
    }
  }
}