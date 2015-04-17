package quickcheck

import common._

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def toList(h: H): List[A] = if (isEmpty(h)) Nil else findMin(h) :: toList(deleteMin(h))

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == a.min(b)
  }

  property("min3") = forAll { a: Int =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("min4") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)
    val h = meld(h1, h2)
    findMin(h) == a.min(b)
  }

  property("min5") = forAll { h: H =>
    val heapValues = toList(h)
    heapValues == heapValues.sorted
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { h: H =>
    val heapValues = toList(h)
    heapValues == heapValues.sorted
  }

  property("gen3") = forAll { (h1: H, h2: H) =>
    val heapValues = toList(meld(h1, h2))
    heapValues.diff(toList(h1) ++ toList(h2)).isEmpty
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield meld(empty, insert(v, h))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
