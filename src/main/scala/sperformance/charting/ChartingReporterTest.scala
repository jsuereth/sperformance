package sperformance
package charting

import intelligence._

import collection.mutable.ListBuffer
import org.jfree.data.xy.{XYSeriesCollection, XYSeries}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartUtilities, ChartFactory}
import java.io.File

trait ChartSeriesCollector {
  def collect(result : PerformanceTestResult) : Unit
}
class SizeChartSeriesCollector(val module : String, val method : String) extends XYSeries(module + "-" + method, true, true) {
}

object SizedResult {
  def unapply(result : PerformanceTestResult) : Option[(String,String,Int,Long)] = {
    lazy val module = result.attributes.get("module").asInstanceOf[Option[String]]
    lazy val method = result.attributes.get("method").asInstanceOf[Option[String]]
    lazy val size = result.axisData.get("size").asInstanceOf[Option[Int]]
    if(module.isDefined  && method.isDefined && size.isDefined) {
      Some((module.get, method.get, size.get, result.time))
    } else None
  }
}

trait ChartingReporterTest extends PerformanceTest with ClusteringTest {

  override def runTest() : Unit  = {
    super.runTest()
    Charting.createReports()
  }

  object Charting {
    val chartGenerators : Seq[ChartGenerator] = SizeChartGenerator :: Nil

    def createReports() {
      //Find data...
      for {
        (_, cluster) <- clusters
        gen <- chartGenerators
        if(gen.canHandleCluster(cluster))
      } {
        gen.generateChart(cluster)
      }
    }
  }
}