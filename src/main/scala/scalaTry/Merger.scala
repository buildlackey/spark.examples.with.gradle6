package scalaTry

class Merger[T]()(implicit e: T => Ordered[T]) {
  def compare(a: T, b: T): Unit = {
    if (a < b)
      println("a less")
    else
      println("a NOT less")
  }

  final def merge(a: List[T],
                  b: List[T]): List[T] = {
    (a,b) match{
      case (Nil,Nil) =>
        Nil
      case (a1::tail, Nil) =>
        a1::tail
      case (Nil, b1::tail) =>
        b1::tail
      case (a1::tail1, b1::tail2) =>
        if (a1 < b1) {
          a1::b1::merge(tail1,tail2)
        } else {
          b1::a1::merge(tail1,tail2)
        }
      case default =>
        throw new
            IllegalStateException(
              "Should never get here")
    }
  }

  def merge2(a: List[T], b: List[T]): List[T] = {
    @scala.annotation.tailrec
    def merge2(a: List[T],
               b: List[T],
               prefixSoFar: List[T]): List[T] = {
      (a,b) match{
        case (Nil,Nil) =>
          prefixSoFar
        case (a1::tail, Nil) =>
          prefixSoFar ::: a1 :: tail
        case (Nil, b1::tail) =>
          prefixSoFar ::: b1 :: tail
        case (a1::tail1, b1::tail2) =>
          if (a1 < b1) {
            val prefix: List[T] =
              prefixSoFar ::: a1 :: b1 :: Nil
            merge2(tail1, tail2, prefix)
          } else {
            val prefix: List[T] =
              prefixSoFar ::: b1 :: a1 :: Nil
            merge2(tail1, tail2, prefix)
          }
        case default =>
          throw new
              IllegalStateException(
                "Should never get here")
      }
    }

    merge2(a, b, Nil)
  }
}
