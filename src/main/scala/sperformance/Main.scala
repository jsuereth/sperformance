package sperformance

import java.io.File


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


  def main(args : Array[String]) {
    //TODO - determine command line settings, and go do them!
    runTestsReflectively(args.map( arg => Class.forName(arg).asInstanceOf[Class[_ <: PerformanceTest]]) : _*)
  }
}