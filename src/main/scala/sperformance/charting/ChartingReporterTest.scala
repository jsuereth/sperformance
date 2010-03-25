package sperformance
package charting

import intelligence._



//hide all our variables in this object
object Charting {
  val default_chartGenerators : Seq[ChartGenerator] = SizeChartGenerator :: Nil

  def createReports(clusters : Map[ClusterMetaData, Cluster], context : RunContext)(implicit chartGenerators : Seq[ChartGenerator] = default_chartGenerators) {
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
