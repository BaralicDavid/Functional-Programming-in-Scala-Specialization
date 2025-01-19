package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = heap

  given Arbitrary[H] = Arbitrary(genHeap)

  def heap: Gen[H] =
    for
      n <- arbitrary[Int]
      kind <- arbitrary[Int]
      heap <- if kind % 3 == 0 then const(empty) else genHeap
    yield
      insert(n, heap)

  property("min1") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOfTwo") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == ord.min(a, b)
  }

  property("insertDelete") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("sortedSeq") = forAll { (h: H) =>
    def sort(h: H): List[A] =
      if isEmpty(h) then Nil
      else findMin(h) :: sort(deleteMin(h))

    val sortedElements = sort(h)
    sortedElements == sortedElements.sorted(ord)
  }

  property("meldOfTwo") = forAll { (h1: H, h2: H) =>
    if !isEmpty(h1) && !isEmpty(h2) then
      val minH1 = findMin(h1)
      val minH2 = findMin(h2)
      val meldedHeap = meld(h1, h2)
      findMin(meldedHeap) == ord.min(minH1, minH2)
    else
      true
  }

  property("meldOfTwoHeapsWithMovedMinimum") = forAll { (h1: H, h2: H) =>
    @tailrec
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))

    heapEqual(meld(h1, h2), meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

