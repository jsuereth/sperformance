package sperformance
package intelligence

/**
 * A test the automatically clusters results...
 */
trait ClusteringTest extends PerformanceTest {

  override implicit val handler = new ClusterResults

  def clusters = handler.clusters
  
}