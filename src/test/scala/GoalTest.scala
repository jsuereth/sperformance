import sperformance.store._
import java.io.File

object GoalTest extends sperformance.dsl.PerformanceDSLTest {
  performance of "List" in {
    measure method "foreach" in {
      withSize upTo 1000 by 100 withSetup { size =>
        (1 to size).toList
      } run { collection =>
        var tmp = 0
        collection.foreach(x => tmp + x)
      } 
    }
  }
  
  def main(args:Array[String]) {
    val context = new sperformance.HistoricalRunContext(new File("target/sperformance/historical"), new XmlStoreResults(_), new XmlLoadResults(_))
    runTest(context)
    
    context.generateResultsPage()

  }
}