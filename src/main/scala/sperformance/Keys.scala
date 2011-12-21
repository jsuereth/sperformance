package sperformance

/**
 * Lists some attribute keys that have special meaning.
 *
 * @note perhaps one day the keys should by typed to their value but could be overkill
 */
object Keys {
  /**
   * A key for Historical Run Context, all versions of the same module and methods
   * will be compared together in a single graph
   */
  val Version = "version"
  /**
   * A key for Historical Run Context, when loading old data the baseline from the old and new will be compared
   * to determine how much to scale the version results.
   */
  val Baseline = "baseline"
  /**
   * The number of warm-up runs used to warm up the JVM before running the test
   */
  val WarmupRuns = "warm-up runs"
  /**
   * The number of times to run the tests before combining them into a single
   * result (depending on how they are combined)
   */
  val TestRuns = "test-runs"
}