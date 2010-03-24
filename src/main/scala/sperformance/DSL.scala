package sperformance

import scala.reflect.Manifest
import collection.mutable.ListBuffer
import generators._

trait PerformanceDSLTest extends PerformanceTest {

  //DSL State variables... YUK
  private var _current_module : Option[String] = None
  private var _current_method : Option[String] = None
  protected def current_module : String = _current_module getOrElse error("Cannot test outside of defined module!")
  protected def current_method : String = _current_method getOrElse error("Cannot test outside of defined module!")
  /**
   * Ensures correct current_module reference during some closure
   */
  private def withCurrentModule[A](module : String)(f : => A) = {
    _current_module = Some(module)
    f
    _current_module = None
  }

  /**
   * Ensures correct current_method reference during some closure
   */
  private def withCurrentMethod[A](method : String)(f : => A) = {
    _current_method = Some(method)
    f
    _current_method = None
  }
  /**
   * Delays execution of some function until later 
   */
  private final class DelayedRunner[A](module : String, method : String, function : () => A) {
    def execute() = {
      withCurrentModule(module) {
        withCurrentMethod(method) {
          function()
        }
      }
    }
  }
  /** Stores delayed DSL-ish tasks until runTest is called */
  private val delayedTasks = new ListBuffer[DelayedRunner[_]]

  /**
   * Delays a given closure until runTest is called
   */
  private def delayUntilTest[A](f : => A) {
    delayedTasks append new DelayedRunner(current_module, current_method, () => f)
  }
  private def executeDelayedTasks() : Unit =  for(task <- delayedTasks) task.execute()
  /**
   * This method will execute a performance test.
   */
  override def runTest(context : RunContext) : Unit  = {
    executeDelayedTasks()
    super.runTest(context);
  }


  /**
   * Start of performance DSL
   */
  object performance {
    def of(module : String) = new {
      def in[A](f : => A) {
         withCurrentModule(module)(f)
      }
    }
    def of[A : Manifest] = new {
      def in[B](f : => B) {
        withCurrentModule(implicitly[scala.reflect.Manifest[A]].erasure.getCanonicalName)(f)
      }
    }
  }

  /**
   * Start of method performance DSL.  Must be wrapped in outer performance DSL
   */
  object measure {
    def method(methodName : String) = new {
      def in[A](f : => A) {
        withCurrentMethod(methodName)(delayUntilTest(f))
      }
    }
  }

  /** Creates generator for size 1 to max */
  object withSize {
    def upTo(max : Int) = new {
      def withSetup[A](f : Int => A) = new SizeGenerator(1, max, current_module, current_method, f)
    }
  }
}