package sperformance
package charting

import intelligence.Cluster

trait ChartGenerator {
  //This method needs *far* more info!!!!
  def generateChart(cluster : Cluster, context : RunContext) : Unit
  def canHandleCluster(cluster : Cluster) : Boolean
}