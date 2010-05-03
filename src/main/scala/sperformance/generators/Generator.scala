package sperformance
package generators

import util.PerformanceTestHelper._

/**
 * This represents the smallest quantum of a performance test
 */
trait PerformanceTestRun[S] {
  def setup : () => S
  def test(s : S) : Unit
  /** Apply whatever mods we need to the context for this specific test... e.g. Adding attribtues */
  def modifyContext(ctx : PerformanceTestRunContext) : PerformanceTestRunContext

  def run(ctx  : PerformanceTestRunContext) : Unit
}

/** Simplest implementation of a performance test */
trait AbstractPerformanceTestRun[S] extends  PerformanceTestRun[S] {
  def run(ctx  : PerformanceTestRunContext) : Unit = {
    val s = setup()
    val result = measure(() => test(s))
    modifyContext(ctx).reportResult(PerformanceTestResult(result, Map(), Map()));
  }
}

/**
 * Interface for performance test generators. A  Generator is used to create any number of performance tests for a given type.
 * Generators also have a mechanism to generate a "warmup" test which is used in an attempt to get hotspot to optimize a
 * particular code path before measuring its execution.
 *
 * Generators are also required to modify the current test context with axis values relating to the generated types from
 * this generator.   The informatino is used in clustering and graphing of performance results over a given variable.
 * The type of the generator is assumed to be an independent variable for graphing.
 *
 * Note: Generators are monadic in nature.
 */
trait Generator[T] {
  /**
   * Creates a single performance test run that can be used to "warm up" the JVM hotspot for warm performance testing.
   */
  def genWarmUp[S](setup : T => S)(test : S => Unit) : PerformanceTestRun[S]

  /**
   * Generates a Traversable of the performance tests that need to be executed.
   *
   * TODO - TraversableView?
   */
  def genTests[S](setup : T => S)(test : S => Unit) : Traversable[PerformanceTestRun[S]]


  /**
   * Runs tests with the given setup method and testing method in a given context.
   *
   * @param S      The type the setup method returns, and the test method expects.  Consider the "testing state type"
   * @param setup  A method that will create initial state for the test
   * @param f      The method to be measured for performance
   */
  def runTests[S](setup : T => S)(f : S => Unit)(ctx : PerformanceTestRunContext) : Unit

  /**
   * Maps a generator of type T to a generator of type U.  This mapping is done during the setup phase (and not quantumed)
   * @param U the new type to generate tests for
   */
  def map[U](f : T=>U) : Generator[U]

  /**
   * Takes a function converting this generator's type into another Generator and returns an flattened view of the new Generator type.
   */
  def flatMap[U](f : T => Generator[U]) : Generator[U]




}

/** Abstract implementations of Generator Operations that are generic */
trait GeneratorOperations[T] extends Generator[T]{

  override def runTests[S](setup : T => S)(f : S => Unit)(ctx : PerformanceTestRunContext) : Unit = {
    Console.println("Running generator " + this + " on ctx " + ctx)

     //Warmup JVM
    val warmUpTest = genWarmUp(setup)(f)
    warmUpJvm(() => warmUpTest.run(NullPerformanceTestRunContext))
    //Execute Tests
    genTests(setup)(f).foreach(_.run(ctx))
  }


  override def map[U](f : T=>U) : Generator[U] = new MappedGenerator(this,f)
  override def flatMap[U](f : T => Generator[U]) : Generator[U] = new NestedGenerator(this, f)
}

/** Maps a generator to another type using a transform function
 *  @param T   The new type generated
 *  @param U   The original generated type.
 *  @param g   The original generator
 *  @param transform   The function that modified the type generated
 */
private [generators] final class MappedGenerator[T,U](g : Generator[T], transform : T => U) extends GeneratorOperations[U] {
  override def genWarmUp[S](setup : U => S)(test : S => Unit) : PerformanceTestRun[S] = g.genWarmUp( transform andThen setup)(test)
  override def genTests[S](setup : U => S)(test : S => Unit) : Traversable[PerformanceTestRun[S]] = g.genTests(transform andThen setup)(test)

  override def toString : String = "MappedGenerator(" + g + ")"
}

/** Nests one generator in another.   Creates a new generator that will contain cross-product of all generated tests.
 *  @param T   The original Generated Type
 *  @param U   The new generated type
 *  @param g   The original generator
 *  @param f   The transformation function to take a generated type T and create a new Generator of U tests.
 */
private[generators] final class NestedGenerator[T,U](g : Generator[T], f : T=>Generator[U]) extends GeneratorOperations[U] {  
  override def genWarmUp[S](setup : U => S)(test : S => Unit) : PerformanceTestRun[S] = {
    //Steal value of warmUp early....
    val warmUp = g.genWarmUp( (t : T) => t)( ignore => ())
    //Now delegate!
    f(warmUp.setup()).genWarmUp(setup)(test)
  }
  override def genTests[S](setup : U => S)(test : S => Unit) : Traversable[PerformanceTestRun[S]] = for {
    initialTest <- g.genTests(identity)(ignore => ())
    secondTest <- f(initialTest.setup()).genTests(setup)(test)
  } yield new AbstractPerformanceTestRun[S] {
    def setup : () => S = secondTest.setup
    def test(s : S) : Unit = secondTest.test(s)
    def modifyContext(ctx : PerformanceTestRunContext) : PerformanceTestRunContext = {
      secondTest.modifyContext(initialTest.modifyContext(ctx))
    }
  }

  override def toString : String = "NestedGenerator(" + g + " mapped by" + f + ")"

}