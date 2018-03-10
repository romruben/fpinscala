package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k: Int <- arbitrary[Int]
      h: H <- arbitrary[H]
    } yield insert(k, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should get the smallest
    * of the two elements back.
    */
  property("insert") = forAll { (a: Int, b: Int) =>
    val min = if (a <= b) a else b
    val h = insert(a, empty)
    findMin(insert(b, h)) == min
  }

  /**
    * If you insert an element into an empty heap,
    * then delete the minimum, the resulting heap should be empty.
    */
  property("delete") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  /**
    * Given any heap, you should get a sorted sequence of elements
    * when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("deleteMin") = forAll { (a: Int, b: Int) =>
    val man = if (a < b) b else a
    val h = insert(a, empty)
    val hAll = insert(b, h)

    findMin(deleteMin(hAll)) == man
  }

  /**
    * Finding a minimum of the melding of any two heaps
    * should return a minimum of one or the other.
    */
  property("meld") = forAll { (a: Int, b: Int) =>
    val min = if (a <= b) a else b
    val h1 = insert(a, empty)
    val h2 = insert(b, empty)

    val meldHeap = meld(h1, h2)
    findMin(meldHeap) == min

  }

  private def contains(h: H, i: Int): Boolean = {
    if(isEmpty(h)) false
    else{
      if (findMin(h) == i) true
      else contains(deleteMin(h), i)
    }
  }

  property("contains") = forAll { (h1: H, i: Int) =>
    val h2 = insert(i, h1)
    contains(h2, i)
  }

}
