package sperformance

import collection.immutable.List
import collection.immutable.Vector
import collection.{SeqView, Traversable}
import collection.mutable.{IndexedSeqView, ArrayBuffer, ListBuffer, LinkedList}

trait TraverableTestHelper extends sperformance.dsl.PerformanceDSLTest {


  def makeTraversableTest[T[X]](setup : (Int) => T[Int])(implicit ev0 : ClassManifest[T[Int]], conv : T[Int] => Traversable[Int]) = {
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

      measure method "foldRight" in {
        withSize upTo 1000 withSetup setup run { collection =>
          var tmp = collection.foldRight(0)(_+_)
          tmp
        }
      }

      measure method "isEmpty" in {
        withSize upTo 1000 withSetup setup run { collection =>
          collection.isEmpty
        }
      }


      measure method "filter" in {
        withSize upTo 1000 withSetup setup run { collection =>
          collection.filter(_%2==0)
        }
      }

      measure method "collect" in {
        withSize upTo 1000 withSetup setup run { collection =>
          collection.collect {
            case x : Int if x % 2 == 0 => x
          }
        }
      }

      measure method "parition" in {
        withSize upTo 1000 withSetup setup run { collection =>
          collection.partition(_%2==0)
        }
      }

      measure method "groupBy" in {
        withSize upTo 1000 withSetup setup run { collection =>
          collection.groupBy(_ % 3)
        }
      }

      measure method "filterAndHeadOption" in {
        withSize upTo 1000 withSetup setup run { collection =>
          collection.filter(_ % 2 == 0).headOption
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
  makeTraversableTest[Vector] { size =>
    var collection : Vector[Int] = Vector[Int]()
    for( i <- 1 to size) collection = collection :+ i //collection = collection.updated(i,i)
    collection
  }

  makeTraversableTest[LinkedList] { size =>
    var collection = LinkedList[Int]()
    collection = collection ++ (1 to size).toList
    collection
  }

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
  makeTraversableTest[Traversable] { size =>
    val tmp = ArrayBuffer.range(1,size)
    new Traversable[Int] {
      def foreach[U](f : Int => U) = tmp.foreach(f)
    }
  }
  makeTraversableTest[({type X[A] = SeqView[A, Seq[A]]})#X] { size =>
    List.range(1, size).toList.view
  }
  makeTraversableTest[IndexedSeq] { size =>
    ArrayBuffer.range(1, size)
  }

  makeTraversableTest[({type X[A] = IndexedSeqView[A, IndexedSeq[A]]})#X] { size =>
    ArrayBuffer.range(1, size).view
  }


  //TODO - We need to test java collections directly... but use similar names for comparison...
  import collection.JavaConversions

  //  makeTraversableTest[java.util.HashSet] { size =>
  //    val x = new java.util.HashSet[Int]
  //    for(i <- 1 to size) x.add(i)
  //    x
  //  }(implicitly[Manifest[java.util.HashSet[Int]]], JavaConversions.asIterable(_ : java.util.Collection[Int]))
  //makeTraversableTest[Set] { size => (1 to size).toSet }


}

object ViewShootOut2 extends sperformance.dsl.PerformanceDSLTest {
  import collection.mutable.ArrayBuffer
  val maxSize = 1000
  def setup(size : Int) = {
    val data : Iterable[Int] = ArrayBuffer.range(1,size)
    new Iterable[Int] {
      override def iterator : Iterator[Int] = data.iterator
    }
  }
  performance of "views" in {
    measure method "takeWithMap" in {
      withSize upTo maxSize withSetup setup run { col =>
        col.view.map(_ * 2).take(100).force
      }
    }
    measure method "takeWithFilter" in {
      withSize upTo maxSize withSetup setup run { col =>
        col.view.filter(_ % 2 == 0).take(100).force
      }
    }
    measure method "takeWithZipAndFilter" in {
      withSize upTo maxSize withSetup setup run { col =>
        col.view.zipWithIndex.filter(_._2 % 2 == 0).take(100).force
      }
    }
  }
  performance of "non-views" in {
    measure method "takeWithMap" in {
      withSize upTo maxSize withSetup setup run { col =>
        col.map(_ * 2).take(100)
      }
    }
    measure method "takeWithFilter" in {
      withSize upTo maxSize withSetup setup run { col =>
        col.filter(_ % 2 == 0).take(100)
      }
    }
    measure method "takeWithZipAndFilter" in {
      withSize upTo maxSize withSetup setup run { col =>
        col.zipWithIndex.filter(_._2 % 2 == 0).take(100)
      }
    }
  }
}


object ViewShootOut extends sperformance.dsl.PerformanceDSLTest {
  import collection.mutable.ArrayBuffer
  import collection.generic.GenericCompanion
  def setup[CC[X] <: Traversable[X]](gg : GenericCompanion[CC])(size : Int) : CC[String] =  {
    gg(  (1 to size).map(_.toString).toSeq : _* )
  }
  performance of "Vector" in {
    measure method "zipWithIndexFilterAndMap" in {
      withSize upTo 1000 withSetup setup(Vector) run { a =>
        a.zipWithIndex.filter(x => x._1.startsWith("1") && (x._2%3==0)).map(_._1)
      }
    }
    measure method "filterAndReduce" in {
      withSize upTo 1000 withSetup setup(Vector) run { a =>
        a.filter(_.startsWith("1")).foldLeft("")((prev, cur) => cur)
      }
    }
  }
  performance of "Vector.view" in {
    measure method "zipWithIndexFilterAndMap" in {
      withSize upTo 1000 withSetup setup(Vector) run { a  =>
        a.view.zipWithIndex.filter(x => x._1.startsWith("1") && (x._2%3==0)).map(_._1).force
      }
    }
    measure method "filterAndReduce" in {
      withSize upTo 1000 withSetup setup(Vector) run { a =>
        a.view.filter(_.startsWith("1")).foldLeft("")( (prev, cur) => cur)
      }
    }
  }
  performance of "ArrayBuffer" in {
    measure method "zipWithIndexFilterAndMap" in {
      withSize upTo 1000 withSetup setup(ArrayBuffer) run { a =>
        a.zipWithIndex.filter(x => x._1.startsWith("1") && (x._2%3==0)).map(_._1)
      }
    }
    measure method "filterAndReduce" in {
      withSize upTo 1000 withSetup setup(ArrayBuffer) run { a =>
        a.filter(_.startsWith("1")).foldLeft("")( (prev, cur) => cur)
      }
    }
  }
  performance of "ArrayBuffer.view" in {
    measure method "zipWithIndexFilterAndMap" in {
      withSize upTo 1000 withSetup setup(ArrayBuffer) run { a  =>
        a.view.zipWithIndex.filter(x => x._1.startsWith("1") && (x._2%3==0)).map(_._1).force
      }
    }
    measure method "filterAndReduce" in {
      withSize upTo 1000 withSetup setup(ArrayBuffer) run { a =>
        a.view.filter(_.startsWith("1")).foldLeft("")( (prev, cur) => cur)
      }
    }
  }
}

