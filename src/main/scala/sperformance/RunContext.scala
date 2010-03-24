package sperformance

import java.io.File
import org.jfree.chart.{ChartUtilities, JFreeChart}

/**
 * Abstract interface designed to allow customize where reports go and how they are generated. (i.e. could be sent to a Swing UI).
 *
 * This interface is by no means complete.
 */
trait RunContext {
  def writeResultingChart(clusterName : List[String], chartName : String, chart : JFreeChart) : Unit
}

//This just dumps charts into output directories...
class DefaultRunContext(val outputDirectory : File) extends RunContext {

  val defaultChartHeight = 500
  val defaultChartWidth = 500

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
  }
}