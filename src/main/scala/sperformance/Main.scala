package sperformance

import java.io.File


object Main {
  implicit var context = new DefaultRunContext(new File("target/sperformance"))


  def runTestsReflectively(tests: Class[_ <: PerformanceTest]*)(implicit context : RunContext) {
    //TODO - ClassLoader magic....
    runTests(tests.map(_.newInstance) : _* )(context)
  }

  def runTests(tests : PerformanceTest*)(implicit context : RunContext) {
    for(test <- tests) {
      test.runTest(context)
    }
  }


  def main(args : Array[String]) {
    //TODO - determine command line settings, and go do them!
    runTestsReflectively(args.map( arg => Class.forName(arg).asInstanceOf[Class[_ <: PerformanceTest]]) : _*)
  }
}