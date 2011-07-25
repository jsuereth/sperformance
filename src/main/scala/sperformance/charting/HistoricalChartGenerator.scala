package sperformance.charting
import sperformance.intelligence.Cluster
import sperformance.RunContext

object HistoricChartGenerator extends ChartGenerator {
  val supportedSubGenerators = SizeChartGenerator :: Nil
  def canHandleCluster(cluster : Cluster) = supportedSubGenerators.exists(_.canHandleCluster(cluster))
  def generateChart(cluster : Cluster, context : RunContext) {
    
  }

}