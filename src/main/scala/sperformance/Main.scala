package sperformance

import java.io.{FileOutputStream, PrintStream, BufferedOutputStream, File}

object Main {
  var outputDirectory = new File("target/sperformance")


  def runTestsReflectively(tests: Class[_ <: PerformanceTest]*) {
    //TODO - ClassLoader magic....
    runTests(tests.map(_.newInstance) : _* )
  }

  def withOutputFileStream(testName : String)(f : java.io.BufferedWriter => Unit) = {
    //TODO - Use Scala I/O!
    val file = new File(outputDirectory, "tests/" + testName + "-sperf.xml")
    val dir = file.getParentFile
    if(!dir.isDirectory) dir.mkdirs()

    val output = new java.io.BufferedWriter(new java.io.FileWriter(file))
    try {
      f(output)
    } finally {
      output.close();
    }
  }
  
  def runTests(tests : PerformanceTest*) {
    import _root_.sperformance.codec._
    val codec : OutputCodec = new XmlOutputCodec(new File(outputDirectory,"tests"))
    for(test <- tests) {
      Console.println("Running test " + test.name + " ....")              
      codec.withOutputFileForTest(test.name) {
        testWriter =>
          val context = new PerformanceTestRunContext {
            def reportResult(result : PerformanceTestResult) : Unit = {
              testWriter.writeResult(result)
            }
          }
          test.runTest(context)
      }
    }
    //TODO - MapReduce into clusters
    //TODO - Create Graphs!
  }

  def writeMainSite(tests: PerformanceTest*) {
    val names = tests.map(_.name)


    def content = (<html>
      <head><title>SPerformance Results</title></head>
      <body>
        <h1>SPerformance Results</h1>
        <ul>
          {
             for(name <- names) yield (<li>
               <a href={name+"/index.html"}>{name}</a>
             </li>)
          }
        </ul>
      </body>
    </html>)

    val index = new File(outputDirectory, "index.html")
    val output = new PrintStream(new BufferedOutputStream(new FileOutputStream(index)))
    try {
      output.println(content)
    }  finally {
      output.close()
    }

  }


  def main(args : Array[String]) {
    //TODO - determine command line settings, and go do them!
    runTestsReflectively(args.map( arg => Class.forName(arg).asInstanceOf[Class[_ <: PerformanceTest]]) : _*)
  }
}