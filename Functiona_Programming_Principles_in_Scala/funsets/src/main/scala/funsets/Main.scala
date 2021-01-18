package funsets

import javax.swing.plaf.BorderUIResource.EmptyBorderUIResource

object Main extends App {

  import FunSets._

  //  println(contains(singletonSet(1), 1))
  //  println(contains(singletonSet(5), 1))
//  val evens: FunSet = (x: Int) => x % 2 == 0
//
//  val p: Int => Boolean = (x: Int) => x % 5 == 0 && x > 0 && x < 100
//  val tens: FunSet = filter(evens, p)
//
//  val emptySet: FunSet = _ => false
//
//  printSet(map(tens, x => x + 100))

  val t1 = new NonEmpty(1, new Empty, new Empty)
  val t2 = new NonEmpty(2, new Empty, new Empty)

  val t3 = t1 union t2

  println(t3)
}

abstract class IntSet {
  def contains(x: Int): Boolean

  def incl(x: Int): IntSet

  override def toString: String = "."

  def union(other: IntSet): IntSet
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  override def toString: String = "{" + left + elem + right + "}"

  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
}

class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  def union(other: IntSet): IntSet = other
}

