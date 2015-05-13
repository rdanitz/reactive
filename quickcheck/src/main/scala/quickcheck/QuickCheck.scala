package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: A =>
    findMin(insert(a, empty)) == a
  }

  property("insert1") = forAll { (a: A, h: H) =>
    findMin(insert(a, h)) == List(a,findMin(h)).min
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == List(findMin(h1), findMin(h2)).min
  }

  property("deleteMin1") = forAll { (a: A, h: H) =>
    if (a <= findMin(h))
      deleteMin(insert(a, h)) == h
    else
      deleteMin(insert(a, h)) != h
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
