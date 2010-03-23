package sperformance
package generators

/** Interface for generators */
trait Generator[T] {
  val module : String
  val method : String
  def run(f : T => Unit)(implicit handler : ReportHandler) : Unit
}