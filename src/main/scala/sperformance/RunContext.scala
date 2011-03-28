package sperformance

import charting.Charting
import intelligence.Cluster
import org.jfree.chart.{ChartUtilities, JFreeChart}
import collection.mutable.ListBuffer
import java.io.{FileOutputStream, BufferedOutputStream, PrintStream, File}
import util.FileUtils
import java.net.URL
import scalax.io.Resource
import scalax.io.Codec

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
    //TODO - more cleverness about how we make charts?
    Charting.createReports(testContext.clusters, this)

    // order is important resultsPage depends on charts the fact that charts
    // have already been created
    val content = resultsPage
    val index = new File(outputDirectory, "index.html")
    FileUtils.ensureDirectoryExists(index)
    val output = new PrintStream(new BufferedOutputStream(new FileOutputStream(index)))
    try {
      output.println(content)
    }  finally {
      output.close()
    }
  }
}

/**
 * Generates no charts, instead this writes the results to a file so that tests can be tracked
 * and charted over time
 */
class CSVRunContext(outputFile:File) extends RunContext {

  val testContext = new intelligence.ClusterResults
  def writeResultingChart(clusterName : List[String], chartName : String, chart : JFreeChart) : Unit = {
    // this doesn't write charts
  }

  private def makeSeriesName(cluster:Cluster)(result : PerformanceTestResult) = cluster.makeName(result.attributes)

  def writeResults() = {
    outputFile.getParentFile.mkdirs
    Resource.fromFile(outputFile).writer(Codec.UTF8).acquireFor {
      writer =>
        for {
          (md, cluster) <- testContext.clusters
          val clusterName = md.attributes.mkString("-")
          (name,results) <- cluster.results.groupBy(makeSeriesName(cluster) _)
          result <- results
            (axisName,axisData) <- result.axisData
        } {
          val rowData = (clusterName, name,axisName,axisData,result.time)
          val rowAsStrings = rowData.productIterator map {_.toString} map {_.replace("\"","\\\"")}
          writer.write(rowAsStrings mkString ("\"","\",\"","\""))
          writer.write("\n")
        }
    }

  }
}