package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(s"${pascal(col, row)} ")
    //      println()
    //    }
    //    println(balance("()".toList))
    //    println(balance("(())".toList))
    //    println(balance("(if (zero? x) max (/ 1 x))".toList))
    //    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    //    println(balance(":-)".toList))
    //    println(balance("())(".toList))
    //    println(balance(")(".toList))
    println(countChange(4, List(1, 2, 3)))
    println(countChange(4, List(1, 2)))
    println(countChange(7, List(2, 3, 5)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) {
      1
    } else {
      pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def isBalanced(braces: List[Char], stack: List[Char]): List[Char] = {
      if (braces.isEmpty) {
        stack
      } else if (stack.isEmpty && braces.nonEmpty) {
        isBalanced(braces.tail, braces.head :: stack)
      } else if (stack.head == '(' && braces.head == ')') {
        isBalanced(braces.tail, stack.tail)
      } else {
        isBalanced(braces.tail, braces.head :: stack)
      }
    }

    isBalanced(chars.filter(p => p == '(' || p == ')'), List()).isEmpty
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    val sortedCoins = coins.sorted.distinct

    def change(money: Int, sortedCoins: List[Int]): Int = {
      if (money == 0) {
        1
      } else if (money < 0) {
        0
      } else if (money >= 1 && sortedCoins.isEmpty) {
        0
      } else {
        change(money, sortedCoins.tail) + change(money - sortedCoins.head, sortedCoins)
      }
    }

    change(money, sortedCoins)
  }
}
