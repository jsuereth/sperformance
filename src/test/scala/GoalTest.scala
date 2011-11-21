import sperformance.store._
import java.io.File
import sperformance.Keys

object GoalTest extends sperformance.dsl.PerformanceDSLTest {

  performance of "List" in {
      having attribute (Keys.WarmupRuns -> 3) in {
        having attribute (Keys.TestRuns -> 6) in {
    measure method "foreach" in {
          withSize from 100 upTo 1000 by 100 withSetup { size =>
            (1 to size).toList
          } run { collection =>
            var tmp = 0
            collection.foreach(x => tmp + x)
          }
        } 
        measure method "foreach" in {
          having attribute (Keys.Version -> "with while") in {
            withSize from 100 upTo 1000 by 100 withSetup { size =>
              (1 to size).toList
            } run { collection =>
              var tmp = 0
              val iter = collection.iterator
              while (iter.hasNext) {
                val next = iter.next()
                tmp + next
              }
            }
          }
        }
        measure method "view foreach" in {
          withSize from 100 upTo 1000 by 100 withSetup { size =>
            (1 to size).toList
          } run { collection =>
            var tmp = 0
            collection.view.foreach(x => tmp + x)
          }
        }
      }
    }
  }

  def main(args: Array[String]) {
    val context = new sperformance.HistoricalRunContext(new File("target/sperformance/historical"), new XmlStoreResults(_), new XmlLoadResults(_))
    runTest(context)

    context.generateResultsPage(name)

  }
}