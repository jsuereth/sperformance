package sperformance

/**
 * Lists some attribute keys that have special meaning.
 *
 * @note perhaps one day the keys should by typed to their value but could be overkill
 */
object Keys {
  /**
   * A key for Historical Run Context, all versions of the same module and methos
   * will be compared together in a single graph
   */
  val Version = "version"
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