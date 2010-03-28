package sperformance
package codec


import java.io._



/** Represents a codec to write out test resutls */
trait OutputCodec {
  trait Context {
    def writeResult(test: PerformanceTestResult) : Unit
  }
  def withOutputFileForTest[A](testName : String)(f : Context => A) : Unit
}


/**
 * The XML variant of an output codec for test resutls
 *
 * TODO - This is the laziest way to serialize and horribly broken!
 */
class XmlOutputCodec(outputDirectory : File) extends OutputCodec {


  private def toXMLString(result : PerformanceTestResult) = {
    "<testResult>\n" +
    "\t<time>" + result.time + "</time>\n" +
    "\t<attributes>\n" +
    result.attributes.toSeq.map({ case(key, value) => "\t\t<"+key+">" +value+"</"+key+">\n" }).mkString(" ") +
    "\t</attributes>\n" +
    "\t<axisData>\n" +
    result.axisData.toSeq.map({ case(key, value) => "\t\t<"+key+">" +value+"</"+key+">\n" }).mkString(" ") +
    "\t</axisData>\n" +
    "</testResult>"
  }

  private class MyContext(outputChannel : BufferedWriter) extends Context {
    def writeResult(test: PerformanceTestResult) : Unit =  {
      outputChannel.write(toXMLString(test))      
    }
  }



  def withOutputFileForTest[A](testName : String)(f : Context => A) : Unit = {
    //TODO - use Scala I/O!
    val file = new File(outputDirectory, testName + "-perf.xml")
    util.FileUtils.withBufferedWriterFor(file) { writer =>
        writer.write("<performanceTest>\n")
        writer.write("<testName>"+testName+"</testName>\n")
        writer.write("<results>\n")
        f(new MyContext(writer))
        writer.write("</results>\n")
        writer.write("</performanceTest>\n")
    }
    
  }

}