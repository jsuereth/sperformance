package sperformance

import collection.Traversable  
import collection.immutable.List
import collection.immutable.Vector
import collection.mutable.ListBuffer


trait TraverableTestHelper extends sperformance.dsl.PerformanceDSLTest {


  def makeTraversableTest[T[X]](setup : (Int) => T[Int])(implicit ev0 : Manifest[T[Int]], conv : T[Int] => Traversable[Int]) = {
    val collectionName = ev0.toString //ev0.erasure.getName

    performance of collectionName in {
      measure method "foreach" in {
        withSize upTo 1000 withSetup setup run { collection =>
          var tmp = 0
          collection.foreach(x => tmp + x)
        }
      }
      measure method "size" in {
        withSize upTo 1000 withSetup setup run { collection =>
          var tmp = 0
          tmp += collection.size
        }
      }

      measure method "head" in {
         withSize upTo 1000 withSetup setup run { collection =>
          var tmp = 0
          tmp += collection.head
        }
      }

      measure method "last" in {
         withSize upTo 1000 withSetup setup run { collection =>
          var tmp = 0
          tmp += collection.last
        }
      }

      measure method "foldLeft" in {
         withSize upTo 1000 withSetup setup run { collection =>
          var tmp = collection.foldLeft(0)(_+_)
          tmp
        }
      }
    }
  }

}

trait JavaCollectionTestHelper extends sperformance.dsl.PerformanceDSLTest {
  def makeJavaCollectionTest[T[X] <: java.util.Collection[X]](setup : (Int) => T[Int])(implicit ev0 : Manifest[T[Int]]) = {
     val collectionName = ev0.toString //ev0.erasure.getName

    performance of collectionName in {

      measure method "size" in {
        withSize upTo 1000 withSetup setup run { collection =>
          var tmp = 0
          tmp += collection.size
        }
      }
    }
  }
}

object ColShootOutTest extends TraverableTestHelper {
  makeTraversableTest[List](size => (1 to size).toList)
  makeTraversableTest[ListBuffer] { size =>
    val collection = new ListBuffer[Int]
    for( i <- 1 to size) collection += i
    collection
  }

  makeTraversableTest[Array] { size => 
    val x = new Array[Int](size)
    for(i <- 1 to size) x(i-1)=i
    x
  }
  makeTraversableTest[Set] { size => (1 to size).toSet }



  //TODO - We need to test java collections directly... but use similar names for comparison...
  import collection.JavaConversions
  
  makeTraversableTest[java.util.HashSet] { size =>
    val x = new java.util.HashSet[Int]
    for(i <- 1 to size) x.add(i)
    x
  }(implicitly[Manifest[java.util.HashSet[Int]]], JavaConversions.asIterable(_ : java.util.Collection[Int]))
  //makeTraversableTest[Set] { size => (1 to size).toSet }
  /*makeTraversableTest[Vector] { size =>
    var collection : Vector[Int] = new Vector[Int](0, size+2, size+2/2)
    for( i <- 1 to size) collection += i //collection = collection.updated(i,i)
    collection
  } */

}

