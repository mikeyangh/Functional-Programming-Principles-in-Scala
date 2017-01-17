package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: A =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("two elements") = forAll { (a: A, b: A) =>
    val m = math.min(a, b)
    val h = insert(b, insert(a, empty))
    findMin(h) == m
  }

  property("insert delete") = forAll { (a: A) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("order") = forAll { (h: H) =>
    def inOrder(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val head = findMin(h)
        val h1 = deleteMin(h)
        if (!isEmpty(h1) && head > findMin(h1)) true
        else inOrder(h1)
      }
    }
    inOrder(h)
  }

  property("melding") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == math.min(findMin(h1), findMin(h2))
  }

  property("insert more") = forAll { (a: A, b: A, c: A) =>
    val min = math.min(math.min(a, b), c)
    val max = math.max(math.max(a, b), c)
    val h = insert(c, insert(b, insert(a, empty)))
    findMin(h) == min && findMin(deleteMin(deleteMin(h))) == max
  }

}
