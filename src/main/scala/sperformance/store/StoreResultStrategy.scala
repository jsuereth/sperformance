package sperformance.store

import sperformance.Keys
import sperformance.PerformanceTestResult
import java.net.URL
import sperformance.intelligence.{Cluster, ClusterResults}
import sperformance.util.FileUtils
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import java.beans.{XMLEncoder,XMLDecoder}
import xml.{Node, XML, NodeSeq}
import sperformance.PerformanceTestRunContext
/**
 * Write a [[sperformance.intelligence.ClusterResults]] to persistent storage
 */

trait StoreResultStrategy {
  def write(results:ClusterResults):Unit
}
trait LoadResultStrategy {
  def read(version:String, testContext:PerformanceTestRunContext):PerformanceTestRunContext
}



class CsvLoadResults(source:URL) extends LoadResultStrategy {
  private def toMap(data:Seq[String]) = {
    val parts = data.map(_ split "->" toSeq).map{case Seq(key,value) => (key,value)}
    Map(parts:_*)
  }
  override def read(version:String, testContext:PerformanceTestRunContext):PerformanceTestRunContext = {
    for ( line <- io.Source.fromURL(source).getLines() ) {
      val Seq(_, time, rest @ _*) = line.split("\",\"").toSeq
      val (axisData,atts) = rest.span(_ != "|")
      val result = PerformanceTestResult(time = time.toLong, axisData = toMap(axisData), attributes = toMap(atts drop 1))
      testContext.reportResult(result)
    }
    testContext
  }
}

class CsvStoreResults(outputFile:File) extends StoreResultStrategy {
  private def makeSeriesName(cluster:Cluster)(result : PerformanceTestResult) = Cluster.makeName(result.attributes)
  private def quote(string:String) = "\""+string.replaceAll("\"","\\\"")+"\""
  def write(results: ClusterResults) = {
    outputFile.getParentFile.mkdirs
    FileUtils.writer(outputFile) {
      writer =>
        for {
          (cluster,i) <- results.clusters.values.zipWithIndex
          (moduleName,results) <- cluster.results.groupBy(makeSeriesName(cluster) _)
          result <- results
        } {
          val atts = result.attributes.toSeq map {case (key,value) => key.toString + "->" + value}
          val axisData = result.axisData.toSeq map {case (key,value) => key.toString + "->" + value}
          val rowData = (i +: result.time +: axisData) ++ ("|" +: atts)
          val rowAsStrings = rowData map {_.toString} map (quote)
          writer.write(rowAsStrings mkString ",")
          writer.write("\n")
        }
    }
  }
}


class XmlStoreResults(outFile:File) extends StoreResultStrategy {
  private def encode(elem:Any):NodeSeq = {
    val out = new ByteArrayOutputStream()
    val enc = new XMLEncoder(out)
    enc.writeObject(elem)
    enc.close()
    out.close()
    (XML loadString out.toString) \ "_"
  }
  private def encMap(data:Map[String,Any]):NodeSeq =
    data.toSeq.map {
      elem =>
        <element>
          <name>{elem._1}</name>
          <value>{encode(elem._2)}</value>
        </element>
    }

  def write(results: ClusterResults) {
    val xml = <clusterResults version="1.0"> {
      for { cluster <- results.clusters.values.toSeq } yield {
        <cluster name={cluster.name}>
          <meta>
            <axis>
              {cluster.metaData.axis.toSeq.map (encode)}
            </axis>
            <atts>
              {encMap(cluster.metaData.attributes)}
            </atts>
          </meta>
          <results>
          {
          cluster.results.toSeq.map {r =>
            <result time={r.time.toString}>
              <attributes>
                {encMap(r.attributes)}
              </attributes>
              <axisData>
                {encMap(r.axisData)}
              </axisData>
            </result>
          }}
          </results>
        </cluster>
      }
    }</clusterResults>
    outFile.getParentFile.mkdirs()
    FileUtils.writer(outFile) {
      writer=>
        XML.write(writer,xml,"UTF-8",true,null)
    }
  }
}

class XmlLoadResults(xmlFile:URL) extends LoadResultStrategy {
  private def readMap(mapRoot:NodeSeq)= Map(mapRoot \\ "element" map{readObj} :_*)
  private def readObj(e:Node) = {    
      val name = (e \\ "name").text
      val value = "<java class=\"java.beans.XMLDecoder\">"+(e \\ "value" \ "_").toString+"</java>"
      val in = new ByteArrayInputStream(value.getBytes("UTF-8"))
      val dec = new XMLDecoder(in)
      val obj = dec.readObject()
      dec.close
      in.close

      name -> obj
  }
  override def read(version:String, testContext:PerformanceTestRunContext): PerformanceTestRunContext = {
    val xml = XML.load(xmlFile)
    (xml \\ "result") foreach { nextResult =>
      val time = (nextResult \\ "@time").text.toLong
      val nonVersionedAtts = readMap(nextResult \ "attributes")
      val versionInfo = nonVersionedAtts.get(Keys.Version) map {value => Keys.Version -> (version + value)} getOrElse (Keys.Version -> version)
      val atts = nonVersionedAtts + versionInfo
      val axisData = readMap(nextResult \ "axisData")
      val report = PerformanceTestResult(time,axisData = axisData, attributes = atts)
      testContext.reportResult(report)
    }
    testContext
  }
}