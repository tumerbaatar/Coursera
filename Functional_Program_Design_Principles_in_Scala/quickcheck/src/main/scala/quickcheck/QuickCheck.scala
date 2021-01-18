package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

import scala.util.{Failure, Success, Try}

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    oneOf(const(empty), {
      for {
        a <- arbitrary[A]
        b <- arbitrary[H]
      } yield insert(a, b)
    })
  }

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def deleteTillEmpty(h: H): List[A] = {
    if (isEmpty(h)) {
      Nil
    } else {
      findMin(h) :: deleteTillEmpty(deleteMin(h))
    }
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insertTwoMinsIntoEmpty") = forAll { (a: A, b: A) =>
    findMin(insert(a, empty)) == a
    isEmpty(deleteMin(deleteMin(insert(b, insert(a, empty)))))
  }

  property("insert_min_into_empty_and_delete_it") = forAll { (a: A) =>
    deleteMin(insert(a, empty)) == empty
  }

  property("continually_delete_min_provides_sorted_sequence") = forAll { (h: H) =>
    val l = deleteTillEmpty(h)
    l == l.sorted(ord)
  }

  property("minimum_of_melding_heaps") = forAll { (h1: H, h2: H) =>
    val meldedHeap = meld(h1, h2)
    if (isEmpty(meldedHeap)) {
      true
    } else {
      val minOfMeld = findMin(meldedHeap)
      (!isEmpty(h1) && minOfMeld == findMin(h1)) || (!isEmpty(h2) && minOfMeld == findMin(h2))
    }
  }

  property("deleteMin_on_empty") = forAll { (h: H) =>
    if (isEmpty(h)) {
      Try(deleteMin(h)) match {
        case Failure(_) => true
        case _ => false
      }
    } else {
      true
    }
  }

  property("findMin_on_empty") = forAll { (h: H) =>
    if (isEmpty(h)) {
      Try(findMin(h)) match {
        case Failure(_) => true
        case _ => false
      }
    } else {
      true
    }
  }

  property("meld result should preserve the order") = forAll { (h1: H, h2: H) =>
    val meldedHeap = meld(h1, h2)
    val orderOfMeld = deleteTillEmpty(meldedHeap)
    orderOfMeld == orderOfMeld.sorted
  }

  property("meld_should_preserve_the_order") = forAll { (a1: A, a2: A, h1: H, h2: H, h3: H ) =>
    val meldOrder1 = meld(h3, meld(
      insert(a1, h1),
      insert(a2, h2)
    ))
    val meldOrder2 = meld(h3, meld(
      insert(a2, h2),
      insert(a1, h1)
    ))

    deleteTillEmpty(meldOrder1) == deleteTillEmpty(meldOrder2)
  }


  property("meld_should_preserve_the_order2") = forAll { (a1: A, a2: A, a3: A, h1: H, h2: H, h3: H ) =>
    val meldOrder1 = meld(insert(a3, h3), meld(
      insert(a1, h1),
      insert(a2, h2)
    ))
    val meldOrder2 = insert(a3, meld(h3, meld(
      insert(a2, h2),
      insert(a1, h1)
    )))

    deleteTillEmpty(meldOrder1) == deleteTillEmpty(meldOrder2)
  }
}
