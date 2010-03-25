package sperformance
package generators


class IntGenerator(name : String,  startSize : Int, endSize : Int) extends Generator[Int] with GeneratorOperations[Int] {
  private[sperformance] lazy val medianSize = (endSize - startSize) / 2


  private class SizeGeneratorTestRun[S](size : Int, setupFunc : Int => S, testFunc : S => Unit) extends AbstractPerformanceTestRun[S] {
      override def setup = () => setupFunc(size)
      override def test(s : S) : Unit  = testFunc(s)
      /** Apply whatever mods we need to the context for this specific test... e.g. Adding attribtues */
      override def modifyContext(ctx : PerformanceTestRunContext) : PerformanceTestRunContext = {
        ctx addAxisValue (name -> size)
      }
  }

  override def genWarmUp[S](setup : Int => S)(test : S => Unit) : PerformanceTestRun[S] = new SizeGeneratorTestRun(medianSize, setup, test)
  override def genTests[S](setup : Int => S)(test : S => Unit) : Traversable[PerformanceTestRun[S]] =
    for(i <- startSize to endSize) yield new SizeGeneratorTestRun(i, setup,test)


  override def toString : String = "IntGenerator(" + name + ", " + startSize + " to " + endSize + ")"
}