package sperformance
package dsl

import scala.reflect.Manifest
import collection.mutable.ListBuffer
import collection.mutable.Stack
import generators._

trait PerformanceDSLTest extends PerformanceTest {

  //DSL State variables... YUK
  private var _current_context : PerformanceTestRunContext = _
  //Console.println("Current context = " + _current_context)
  private def withNewContext[A](modify : PerformanceTestRunContext => PerformanceTestRunContext)(f : => A) = {    
    val tmp = _current_context
     _current_context = modify(_current_context)
    f
    _current_context = tmp
  }
  /**
   * Ensures correct module/class reported in tests
   */
  private def withCurrentModule[A](module : String)(f : => A) = withNewContext(_.addAttribute("module", module))(f)
  /**
   * Ensures correct method reported in tests
   */
  private def withCurrentMethod[A](method : String)(f : => A) = withNewContext(_.addAttribute("method", method))(f)

  //Current gnerator of tests...
  private val _current_generator = new Stack[Generator[_]]
  private def addGenerator(g : Generator[_]) : Generator[_] = {
    if(!_current_generator.isEmpty) {
      val head = _current_generator.head
      val next = head.asInstanceOf[Generator[Any]].flatMap(ignore => g.asInstanceOf[Generator[Any]])
      _current_generator.push(next)
      next
    } else {
      _current_generator.push(g)
      g
    }
  }
  private def popGenerators() {
    _current_generator.clear()
  }
  private def withCurrentGenerator[A](f : Generator[A] => Unit) = f(_current_generator.head.asInstanceOf[Generator[A]])


  
  /**
   * Delays execution of some function until later
   *
   * TODO - Save generator stack too? 
   */
  private final class DelayedRunner[A](ctx : PerformanceTestRunContext, function : () => A) {
    def execute() = {
      //TODO - Restore to state when delayed?  Startup issues currently....
      //_current_context = ctx
      function()
    }
  }
  /** Stores delayed DSL-ish tasks until runTest is called */
  private val delayedTasks = new ListBuffer[DelayedRunner[_]]

  /**
   * Delays a given closure until runTest is called
   */
  private def delayUntilTest[A](f : => A) {
    delayedTasks append new DelayedRunner(_current_context, () => f)
  }
  private def executeDelayedTasks() : Unit =  for(task <- delayedTasks) task.execute()
  /**
   * This method will execute a performance test.
   */
  override def runTest(context : RunContext) : Unit  = {
    _current_context = context.testContext
    executeDelayedTasks()
    super.runTest(context);
  }


  /**
   * Start of performance DSL
   */
  object performance {
    def of(module : String) = new {
      def in[A](f : => A) {
         delayUntilTest(withCurrentModule(module)(f))
      }
    }
    def of[A : Manifest] = new {
      def in[B](f :  => B) {
        delayUntilTest(withCurrentModule(implicitly[scala.reflect.Manifest[A]].erasure.getCanonicalName)(f))
      }
    }
  }

  /**
   * Start of method performance DSL.  Must be wrapped in outer performance DSL
   */
  object measure {
    def method(methodName : String) = new {
      def in[A](f : => A) {
        withCurrentMethod(methodName)(f)
      }
    }
  }

  /**
   * This class is returned when the DSL is able to make use of the current performance test generator.
   */
  sealed class GeneratorUser[T] {
    def withSetup[A](setup : T => A) = new {
      def run(test : A => Unit) : Unit = {
        withCurrentGenerator[T](_.runTests(setup)(test)(_current_context))
        popGenerators()
      }
    }

    def and[T<: GeneratorDSLStarter](x : T) = x
  }
  /** Marker interface for DSL to continue using "and" */
  sealed trait GeneratorDSLStarter


  sealed trait IntGeneratorDSLStarter extends GeneratorDSLStarter {
    val name : String
    def upTo(max : Int) = {
      addGenerator(new IntGenerator(name,1,max))
      new GeneratorUser[Int]
    }
  }

  /** Creates generator for size 1 to max */
  object withSize extends IntGeneratorDSLStarter {
    override val name = "size"
  }
  /** Creates generator for size 1 to max */
  object withIndex extends IntGeneratorDSLStarter {
    override val name = "index"
  }



}