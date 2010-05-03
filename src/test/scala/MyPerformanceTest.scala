import _root_.sperformance.dsl.PerformanceDSLTest
import collection.mutable.ListBuffer

object MyPerformanceTest extends PerformanceDSLTest {
  performance of "List" in {
    measure method "foreach" in {
      withSize upTo 1000 withSetup { size =>
        (1 to size).toList
      } run { collection =>
        var tmp = 0
        collection.foreach(x => tmp + x)
      }
    }
    measure method "size" in {
      withSize upTo 1000 withSetup { size =>
        (1 to size).toList
      } run { collection =>
        var tmp = 0
        tmp += collection.size
      }
    }
  }
  performance of "ListBuffer" in {
    measure method "foreach" in {
      withSize upTo 1000 withSetup { size =>
        val collection = new ListBuffer[Int]
        for( i <- 1 to size) collection += i
        collection
      } run { collection =>
        var tmp = 0
        collection.foreach(x => tmp + x)
      }
    }
    measure method "size" in {
      withSize upTo 1000 withSetup { size =>
        val collection = new ListBuffer[Int]
        for( i <- 1 to size) collection += i
        collection
      } run { collection =>
        var tmp = 0
        tmp += collection.size
      }
    }
  }
  performance of "Array" in {
    measure method "foreach" in {
      withSize upTo 1000 withSetup { size =>
        val collection = new Array[Int](size)
        for(i <- 1 to size) collection(i-1) = i
        collection
      } run { collection =>
        var tmp = 0
        collection.foreach(x => tmp = x * 20)
      }
    }
    measure method "size" in {
      withSize upTo 1000 withSetup { size =>
        val collection = new Array[Int](size)
        for(i <- 1 to size) collection(i-1) = i
        collection
      } run { collection =>
        var tmp = 0
        tmp += collection.size
      }
    }
  }

   performance of "java.util.ArrayList" in {
     import scala.collection.JavaConversions._
    measure method "foreach" in {
      withSize upTo 1000 withSetup { size =>
        val collection = new java.util.ArrayList[Int](size+1)
        for(i <- 1 to size) collection.add(i)
        collection
      } run { collection =>
        var tmp = 0
        collection.foreach(x => tmp = x * 20)
      }
    }
   measure method "size" in {
      withSize upTo 1000 withSetup { size =>
        val collection = new java.util.ArrayList[Int](size+1)
        for(i <- 1 to size) collection.add(i)
        collection
      } run { collection =>
        var tmp = 0
        tmp += collection.size
      }
    }
  }
}