package sperformance
package charting

import intelligence.Cluster
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
import org.jfree.chart.{ChartFactory}
import org.jfree.chart.plot.PlotOrientation

object SizeChartGenerator extends ChartGenerator {
  val sizeAxisName = "size"

  override def canHandleCluster(cluster : Cluster) : Boolean = {
    cluster.metaData.axis.contains(sizeAxisName) && cluster.metaData.axis.size == 1
  }

  private def makeSeriesName(result : PerformanceTestResult) = result.attributes.values.mkString("-")

  def makeChartName(cluster : Cluster) =  {
    cluster.metaData.attributes.mkString(" & ").replace(">", "") + " By Size"
  }

  def makeSeries(cluster : Cluster) = {
    val seriesCollection = new XYSeriesCollection()
    val groups = cluster.results.groupBy(makeSeriesName)
    for((name, results) <- groups) {
      val series = new XYSeries(name, true, true)
      for(result <- results) {
        series.add(result.axisData(sizeAxisName).asInstanceOf[Int], result.time.asInstanceOf[Long])
      }
      seriesCollection.addSeries(series)
    }
    seriesCollection
  }

  override def generateChart(cluster : Cluster, context :RunContext) : Unit = {
    //Now we rip through cluster data...
    val chartName = makeChartName(cluster)
    val series = makeSeries(cluster)

    //TODO - Fix this!!!
    //First we should compare different modules with similar named methods
    //Second we should compare methods on the same module....
    val chart = ChartFactory.createXYLineChart(chartName, "size", "time", series, PlotOrientation.VERTICAL, true, true, false)
    context.writeResultingChart(List(sizeAxisName),chartName, chart)
  }

}
