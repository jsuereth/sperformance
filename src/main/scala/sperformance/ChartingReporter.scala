package sperformance

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
    lazy val size = result.attributes.get("size").asInstanceOf[Option[Int]]
    if(module.isDefined  && method.isDefined && size.isDefined) {
      Some((module.get, method.get, size.get, result.time))
    } else None
  }
}

trait ChartingReporterTest extends PerformanceTest {

  trait ChartGenerator {
    //This method needs *far* more info!!!!
    def generateCharts() : Unit
  }


  object SizeChartCollector extends ChartGenerator {

    var methodsSeen = Map[(String,String), XYSeries]()
    val sizeSeries = new XYSeriesCollection()

    def collect(module : String, method : String, size : Int, time : Long) {
      if(!methodsSeen.contains((module, method))) {
        val series = new XYSeries(module +"-" + method, true, true)
        sizeSeries.addSeries(series)
        methodsSeen += ( (module -> method ) -> series)
      }
      methodsSeen( (module -> method) ).add(size,time)
    }

     def generateCharts() : Unit = {
       //TODO - Fix this!!!
       //First we should compare different modules with similar named methods
       //Second we should compare methods on the same module....
       val chart = ChartFactory.createXYLineChart("size-based-tests", "size", "time", sizeSeries, PlotOrientation.VERTICAL, true, true, false)
       ChartUtilities.saveChartAsPNG(new File("size-based-tests.png"),chart, 500,500)
     }

  }

  val generators : Seq[ChartGenerator] = SizeChartCollector :: Nil

  override implicit val handler = new ReportHandler {
    def reportResult(result : PerformanceTestResult) : Unit = result match {
      case SizedResult(module,method,size,time) =>
        SizeChartCollector.collect(module,method,size,time)
      case _ => Console.println("Unknown result: " + result)
    }
  }

  override def runTest() : Unit  = {
    super.runTest()
    Charting.createReports()
  }

  object Charting {
    

    def createReports() {
      for(gen <- generators) gen.generateCharts()
    }
  }
}