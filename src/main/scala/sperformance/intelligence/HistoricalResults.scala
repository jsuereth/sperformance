package sperformance.intelligence
import sperformance.PerformanceTestResult

/**
 * @author jeichar
 */
class HistoricalResults extends ClusterResults {
  override def reportResult(result: PerformanceTestResult): Unit = {
    //TODO - Add to appropriate clusters of information, adjust clusters as appropriate...
    val metaData = ClusterMetaData(result.attributes, result.axisData.keySet)
    def clusterForAttr: Option[Cluster] = clusters.get(metaData)
    val cluster: Cluster = clusterForAttr.getOrElse(addAndReturnCluster(new Cluster(metaData)))
    cluster.addResult(result)
  }

}