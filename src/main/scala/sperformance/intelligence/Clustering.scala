package sperformance
package intelligence

import collection.mutable.ListBuffer



/** Metadata about clusters - used to ensure results go to appropriate cluster. */
case class ClusterMetaData(attributes : Map[String,Any], axis : Set[String]) {
  def matchesCluster(result : PerformanceTestResult) : Boolean = {
    def containsAxis = axis.forall(result.axisData.contains)

    def containsAttribtues = attributes.forall { case (key, value) =>
      result.attributes.get(key).map(_ == value).getOrElse(false)
    }
    containsAttribtues && containsAxis
  }
}
/** A cluster of results */
class Cluster(val metaData : ClusterMetaData) {

  val results = new ListBuffer[PerformanceTestResult]


  def addResult(result :PerformanceTestResult)  {
     results append result
  }


  def makeName(attributes : Map[String, Any]) = {
    def sortedAttributeKeys = attributes.keySet.toList.sortWith( (x,y) => x.compareToIgnoreCase(y) >= 0)
    sortedAttributeKeys.flatMap(attributes.get).mkString("", "-","")
  }
}

/**
 * This class is eventually supposed to cluster all results into packets of results.
 *
 * For now we implement just one method cuz we're lazy...
 */
class ClusterResults extends ReportHandler {
  
  var clusters  = Map[ClusterMetaData, Cluster]()

  def addAndReturnCluster(cluster : Cluster) : Cluster = {
    Console.println("Creating cluster: " + cluster.metaData)
    clusters += ((cluster.metaData, cluster))
    cluster
  }

  def reportResult(result : PerformanceTestResult) : Unit = {
      //TODO - Add to appropriate clusters of information, adjust clusters as appropriate...
      for {
        attr <- result.attributes
        (axisName,_) <- result.axisData
        val metaData = ClusterMetaData(Map(attr), Set(axisName))
      } {
        def clusterForAttr : Option[Cluster] = clusters.get(metaData)
        val cluster : Cluster = clusterForAttr.getOrElse(addAndReturnCluster(new Cluster(metaData)))
        cluster.addResult(result)
      }
  }
}