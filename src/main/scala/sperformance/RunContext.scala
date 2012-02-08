package sperformance

import charting.Charting
import intelligence.Cluster
import org.jfree.chart.{ChartUtilities, JFreeChart}
import collection.mutable.ListBuffer
import java.io.{FileOutputStream, BufferedOutputStream, PrintStream, File}
import util.FileUtils
import java.net.URL
import store.StoreResultStrategy
import store.LoadResultStrategy
import sperformance.intelligence.ClusterMetaData
import org.jfree.data.category.CategoryDataset
import org.jfree.data.category.DefaultCategoryDataset
import org.jfree.chart.ChartFactory
import org.jfree.chart.plot.PlotOrientation
import java.awt.Color
import org.jfree.chart.plot.CategoryPlot
import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.renderer.category.BarRenderer
import java.awt.GradientPaint
import org.jfree.chart.axis.CategoryLabelPositions

/**
 * Abstract interface designed to allow customize where reports go and how they are generated. (i.e. could be sent to a Swing UI).
 *
 * This interface is by no means complete.
 */
trait RunContext {
  /** The context to use when running tests */
  def testContext : PerformanceTestRunContext
  def writeResultingChart(clusterName : List[String], chartName : String, chart : JFreeChart) : Unit
  def testFinished(test:PerformanceTest):Unit = {}
  lazy val defaultTestContext = testContext addAttribute
    ("jvm-version", System.getProperty("java.vm.version")) addAttribute
    ("jvm-vendor", System.getProperty("java.vm.vendor")) addAttribute
    ("jvm-name", System.getProperty("java.vm.name")) addAttribute
    ("os-name", System.getProperty("os.name")) addAttribute
    ("os-arch", System.getProperty("os.arch")) addAttribute
    ("cores", Runtime.getRuntime.availableProcessors)
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
 * Creates bar charts that compares the results of several versions for each test ran. 
 * 
 *  Basically it runs the test and collects the results.  When the test is finished it writes
 *  the results of the test in the historyDir/versions/current/filename.xml (in reality it is not
 *  dependent on an xml file but the only useful implementation at the moment is xml so I am hardcoding that
 *  until we have other solutions.)
 *  
 *  Once the files is written the same directoy (historyDir/versions) is checked for other versions and
 *  all others are loaded as well (but with the version information added).  
 *  
 *  All versions of the same module,test and method are grouped together and drawn to a bar chart with the
 *  versions drawn together as a category.  
 *  
 *  Since the chart is a bar chart it can only represent data with a few data points.  For example
 *  a test of size 0 to 1000 is a bad candidate.  On the other hand a test of size 100 to 1000 by 250 is fine
 *  because it only has 4 data points per version.
 * 
 * @param historyDir The directory containing the history of performance tests
 * @param newVersion if true then a new version will be create if false then 
 * 				 	 the latest version will be overridden
 * @param factory    The factory function for creating a StoreResultStrategy 
 */
class HistoricalRunContext(historyDir:File, storeFactory:File => StoreResultStrategy, loadFactory:URL => LoadResultStrategy) extends RunContext {

  val versionsDir = new File(historyDir, "versions")
  val graphsDir = new File(historyDir, "graphs") // probably also needs to be parameterized somehow
  graphsDir.mkdirs()

  val currentVersionKey = "current"
  
  val testContext = new PerformanceTestRunContext {
    val allVersions = new intelligence.HistoricalResults
    val results = new intelligence.HistoricalResults

    override def attribute[U](key: String): Option[U] = results.attribute[U](key)
    override def axisValue[U](key: String): Option[U] = results.axisValue[U](key)

    def reportResult(result: PerformanceTestResult) = {
      allVersions.reportResult(result)
      results.reportResult(result)
    }
  }
  val currentVersionDir = new File(versionsDir, currentVersionKey)

  def writeResultingChart(clusterName: List[String], chartName: String, chart: JFreeChart): Unit = {
    val allVersions = testContext.allVersions

    val grapher = new DefaultRunContext(new File(historyDir.getParentFile(), "graphs"), "unknown") {
      override val testContext = allVersions
    };

    grapher.writeResultingChart(clusterName, chartName, chart)
  }

  def group(md: ClusterMetaData) = md.attributes.filter(n => n._1 == "module" || n._1 == "method")

  def isBaseline(elem: (ClusterMetaData, Cluster)) = elem._1.attributes.exists(_._1 == Keys.Baseline)
  
  def version(atts: Map[String, Any]) = atts.find(_._1 == Keys.Version).map(v => v._2.toString) getOrElse currentVersionKey
  def resultTitle(result: PerformanceTestResult) = result.axisData.head._2.toString

  def baselineTimes:Map[String,Double] = {
    val currentTimes:Map[String,Long] = testContext.results.clusters.filter(isBaseline).flatMap {
      case (md,cluster) => 
        val groupId = group(md).mkString
        cluster.results.map{r =>
          (groupId+r.axisData("size"), r.time)
        }
        
    }
    val allTimes:Map[String,Double] = testContext.allVersions.clusters.flatMap {
      case (md, cluster) =>
        cluster.results.map { result =>
          val groupId = group(md).mkString+result.axisData("size")
          val time = result.time.toDouble / currentTimes.getOrElse(groupId, result.time)
          (groupId+resultTitle(result), time) }
    }.toMap
    allTimes
  }

  def generateResultsPage(chartName: String) {
    val VersionExtractor = """(.+? %% )?(.*)""".r

    def dropVersion(md: ClusterMetaData) = {
      md.attributes.map { case (VersionExtractor(version, att), value) => (att, value) }
    }

    val chartGrouping = testContext.allVersions.clusters.groupBy {
      entry => group(entry._1)
    }.map { case (group, map) => (group, map.map { case (key, value) => value }) }

    def clusterSorterByVersion(r1: Cluster, r2: Cluster): Boolean = {
      def clusterVersion(c: Cluster) = version(c.metaData.attributes)

      val v1 = clusterVersion(r1)
      val v2 = clusterVersion(r2)

      if (v1 == currentVersionKey) false
      else if (v2 == currentVersionKey) true
      else v1.compareToIgnoreCase(v2) < 0
    }
    for ((grouping, value) <- chartGrouping) {
      val dataset = new DefaultCategoryDataset()
      for {
        cluster <- value.toSeq.sortWith(clusterSorterByVersion)
        result <- cluster.results
      } {
        val title = resultTitle(result)
        dataset.addValue(result.time * baselineTimes.getOrElse(grouping.mkString+result.axisData("size")+title,1.0), version(result.attributes), title)
      }

      val name = Cluster.makeName(grouping)
      val chart = ChartFactory.createBarChart(name, // chart title
        value.head.metaData.axis.head, // domain axis label
        "time", // range axis label
        dataset, // data
        PlotOrientation.VERTICAL, // orientation
        true, // include legend
        true, // tooltips?
        false // URLs?
        );

      chart.setBackgroundPaint(Color.white);

      val plot = chart.getPlot().asInstanceOf[CategoryPlot];
      plot.setBackgroundPaint(Color.lightGray);
      plot.setDomainGridlinePaint(Color.white);
      plot.setDomainGridlinesVisible(true);
      plot.setRangeGridlinePaint(Color.white);

      val rangeAxis = plot.getRangeAxis().asInstanceOf[NumberAxis];
      rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());

      // disable bar outlines...
      val renderer = plot.getRenderer().asInstanceOf[BarRenderer];
      renderer.setDrawBarOutline(false);
      /*
                // set up gradient paints for series...
        val gp0 = new GradientPaint(0.0f, 0.0f, Color.blue,
                0.0f, 0.0f, new Color(0, 0, 64));
        val gp1 = new GradientPaint(0.0f, 0.0f, Color.green,
                0.0f, 0.0f, new Color(0, 64, 0));
        val gp2 = new GradientPaint(0.0f, 0.0f, Color.red,
                0.0f, 0.0f, new Color(64, 0, 0));
        renderer.setSeriesPaint(0, gp0);
        renderer.setSeriesPaint(1, gp1);
        renderer.setSeriesPaint(2, gp2);*/

      val domainAxis = plot.getDomainAxis();
      domainAxis.setCategoryLabelPositions(
        CategoryLabelPositions.createUpRotationLabelPositions(Math.Pi / 6.0));
      val chartDir = new File(graphsDir, chartName)
      chartDir.mkdirs
      FileUtils.outputStream(new File(chartDir, name + ".png")) {
        out =>
          ChartUtilities.writeChartAsPNG(out, chart, 800, 600)
      }
      import FileUtils.listFiles
      // print out some html files for browsing the different graphs
      listFiles(graphsDir).foreach { moduleDir =>
        val graphs = listFiles(moduleDir).filter(_.getName.endsWith(".png"))
      }
    }

  }

  /**
   * Write out a version
   */
  def writeVersion(testName: String) = {
    currentVersionDir.mkdirs()
    val testOutputFile = new File(currentVersionDir, testName + ".xml")
    val strategy = storeFactory(testOutputFile)
    strategy.write(testContext.results)
  }

  override def testFinished(test: PerformanceTest): Unit = {
    def list(f: File) = Option(f.listFiles) getOrElse Array[File]()

    writeVersion(test.name)
    val versions = list(versionsDir).filterNot { _.getName == currentVersionDir.getName() }
    for (file <- versions.flatMap { f => list(f).find(_.getName() == test.name + ".xml") }) {
      val version = file.getParentFile().getName()
      loadFactory(file.toURI.toURL).read(version, testContext.allVersions, onlyScalaIOVersions(version))
    }
  }

  def onlyScalaIOVersions(version: String) = (report: PerformanceTestResult) => {
    report.attributes.get(Keys.Version).exists { _ == version } || report.attributes.get(Keys.Baseline).nonEmpty
  }
}