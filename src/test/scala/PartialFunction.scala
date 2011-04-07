package sperformance

import collection.Traversable
import collection.generic.CanBuildFrom


object PartialFunctionTest extends sperformance.dsl.PerformanceDSLTest {
  def collectWithTry[A,B, To](col : Traversable[A])(f : PartialFunction[A,B])(implicit cbf : CanBuildFrom[Traversable[A], B, To]) = {
    val bf = cbf()
    for (x <- col) {
      if (f.isDefinedAt(x)) {
        try bf += f(x)
        catch { case _ : MatchError => () }
      }
    }
    bf.result
  }


  performance of "Traversable" in {
    measure method "collectWithTry" in {
      withSize upTo 1000 withSetup { i =>
        (1 to i) map (j => math.random)
      } run { col =>
        collectWithTry(col)({
          case x if x < 0.5 => "Stringer"
        })(implicitly[CanBuildFrom[Traversable[Double],String,Traversable[String]]])
      }
    }
    measure method "collect" in {
      withSize upTo 1000 withSetup { i =>
        (1 to i) map (j => math.random)
      } run { col =>
        col.collect({
          case x if x < 0.5 => "Stringer"
        })(implicitly[CanBuildFrom[Traversable[Double],String,Traversable[String]]])
      }
    }
  }
}

