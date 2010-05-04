package sperformance

import charting.Charting
import org.jfree.chart.{ChartUtilities, JFreeChart}
import collection.mutable.ListBuffer
import java.io.{FileOutputStream, BufferedOutputStream, PrintStream, File}
import util.FileUtils

/**
 * Abstract interface designed to allow customize where reports go and how they are generated. (i.e. could be sent to a Swing UI).
 *
 * This interface is by no means complete.
 */
trait RunContext {
  /** The context to use when running tests */
  def testContext : PerformanceTestRunContext
  def writeResultingChart(clusterName : List[String], chartName : String, chart : JFreeChart) : Unit
}


//This just dumps charts into output directories...
class DefaultRunContext(val outputDirectory : File, testName : String) extends RunContext {

  val defaultChartHeight = 600
  val defaultChartWidth = 800

  //Funny how all our intelligence is embedded here for generating clusters...
  override val testContext = new intelligence.ClusterResults 

  case class Chart(clusterName : List[String], chartName : String, chartFile: File)

  private val charts = new ListBuffer[Chart]

  def getResultingFile(clusterName : List[String], chartName : String) : File = {
     def relativeFilename = clusterName.mkString(File.separator) + File.separator + chartName + ".png"
    new File(outputDirectory,relativeFilename)
  }

  def writeResultingChart(clusterName : List[String], chartName : String, chart : JFreeChart) {
    val file = getResultingFile(clusterName, chartName)
    //Ensure
    if(!file.exists) {
      val parent = file.getParentFile
      if(!parent.exists) {
        parent.mkdirs
      }
    }
    ChartUtilities.saveChartAsPNG(file, chart,defaultChartWidth, defaultChartHeight)

    //TODO - Write information to cache...
    charts += Chart(clusterName, chartName, file)
  }

  import scala.xml._
  def resultsPage : Node = (<html>
    <head><title>{testName} Results</title></head>
    <style type="text/css">{
""".cluster h1 {
      text-align: center;
    }
    .clusterResults {
     width: 90%;
    }
    .chart {
      float: left;
      clear: none;
      width: 50%;
      text-align: center;
    }"""
         }</style>
    <body>
      {
         for {
           (cluster, charts) <- charts.groupBy( c => c.clusterName)
         } yield <div class="cluster">
           <h1>Graphed By {cluster}</h1>
           <div class="attributes">
             {
                //TODO - Output Cluster attributes that lead to this chart?
             }
           </div>
           <div class="clusterResults">
             {
               for(chart <- charts) yield <div class="chart">
                   <img src={util.FileUtils.relativePath(outputDirectory, chart.chartFile)} alt={"Image - " + chart.chartName}/>
               </div>
             }
           </div>
        </div>
      }
    </body>
  </html>)


  def generateResultsPage() {
    val content = resultsPage
    val index = new File(outputDirectory, "index.html")
    FileUtils.ensureDirectoryExists(index)
    val output = new PrintStream(new BufferedOutputStream(new FileOutputStream(index)))
    try {
      output.println(content)
    }  finally {
      output.close()
    }
    //TODO - more cleverness about how we make charts?
    Charting.createReports(testContext.clusters, this)
  }
}