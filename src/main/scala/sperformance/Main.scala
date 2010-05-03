package sperformance

import java.io.{FileOutputStream, PrintStream, BufferedOutputStream, File}

/**
 * This object is meant to be the final runner of the SPerformance test framework.
 */
object Main {
  var outputDirectory = new File("target/sperformance")


  def runTestsReflectively(tests: Class[_ <: PerformanceTest]*) {
    //TODO - ClassLoader magic....
    runTests(tests.map(_.newInstance) : _* )
  }

  def runTests(tests : PerformanceTest*) {
    for(test <- tests) {
      val context = new DefaultRunContext(new File(outputDirectory,test.name), test.name)
      test.runTest(context)
      context.generateResultsPage()
    }
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