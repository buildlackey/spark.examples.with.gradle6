package scalaTry

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MergeTest extends AnyFunSuite {

  type MergeStringLists = (List[String], List[String]) => List[String]
  type MergeIntLists = (List[Int], List[Int]) => List[Int]

  def doMergeTest (fun1: MergeStringLists,
                   fun2: MergeIntLists): Unit = {
    var res = fun1(List[String](), List[String]())
    assert(res.equals(List[String]()))

    res = fun1(List[String](), "foo" :: List[String]())
    assert(res.equals(List[String]("foo")))


    var res2: List[Int] = fun2(2 :: List[Int](), 5 :: List[Int]())
    assert(res2.equals(List[Int](2, 5)))

    res2 = fun2(5 :: Nil, 2 :: Nil)
    assert(res2.equals(List[Int](2, 5)))

    res2 = fun2(5 :: 6 :: Nil, 6 :: Nil)
    assert(res2.equals(List[Int](5, 6, 6)))

    res2 = fun2(6 :: Nil, 5 :: 6 :: Nil)
    assert(res2.equals(List[Int](5, 6, 6)))


    res2 = fun2(5 :: 6 :: Nil, 4 :: 6 :: Nil)
    assert(res2.equals(List[Int](4, 5, 6, 6)))

    res2 = fun2(3 :: 6 :: Nil, 5 :: 6 :: Nil)
    assert(res2.equals(List[Int](3, 5, 6, 6)))
    ()
  }

  test("merging works") {
    val fun1: MergeStringLists =
      (list1: List[String], list2: List[String]) =>
        new Merger[String]().merge(list1, list2)
    val fun2: MergeIntLists =
      (list1: List[Int], list2: List[Int]) =>
        new Merger[Int]().merge(list1, list2)
    doMergeTest(fun1, fun2)

    val fun3: MergeStringLists =
      (list1: List[String], list2: List[String]) =>
        new Merger[String]().merge2(list1, list2)
    val fun4: MergeIntLists =
      (list1: List[Int], list2: List[Int]) =>
        new Merger[Int]().merge2(list1, list2)
    doMergeTest(fun3, fun4)
  }
}
