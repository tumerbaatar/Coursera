package calculator

import scala.collection.immutable

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {

  def hasCyclicDependencies(targetName: String, expr: Expr, namedExpressions: Map[String, Signal[Expr]]): Boolean = {
    expr match {
      case Ref(name) => if (targetName == name) true else hasCyclicDependencies(targetName, getReferenceExpr(name, namedExpressions), namedExpressions)
      case Literal(_) => false
      case Plus(a, b) => hasCyclicDependencies(targetName, a, namedExpressions) || hasCyclicDependencies(targetName, b, namedExpressions)
      case Minus(a, b) => hasCyclicDependencies(targetName, a, namedExpressions) || hasCyclicDependencies(targetName, b, namedExpressions)
      case Times(a, b) => hasCyclicDependencies(targetName, a, namedExpressions) || hasCyclicDependencies(targetName, b, namedExpressions)
      case Divide(a, b) => hasCyclicDependencies(targetName, a, namedExpressions) || hasCyclicDependencies(targetName, b, namedExpressions)
    }
  }

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {

    val naned = namedExpressions
      .map(item => if (!namedExpressions.contains(item._1)) {
        (item._1, Signal(Literal(Double.NaN)))
      } else {
        item
      })
      .map(item =>
        if (hasCyclicDependencies(item._1, item._2(), namedExpressions)) {
          (item._1, Signal(Literal(Double.NaN)))
        } else {
          item
        }
      )

    naned
      .map {
        case (name, signal) => (name, Signal(eval(signal(), namedExpressions)))
      }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Ref(name) => if (references.contains(name)) {
        eval(references(name)(), references)
      } else {
        Double.NaN
      }
      case Literal(nan) if nan == Double.NaN => Double.NaN
      case Literal(v) => v
      case Plus(a, b) => eval(a, references) + eval(b, references)
      case Minus(a, b) => eval(a, references) - eval(b, references)
      case Times(a, b) => eval(a, references) * eval(b, references)
      case Divide(a, b) => eval(a, references) / eval(b, references)
    }
  }

  /** Get the Expr for a referenced variables.
   * If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Map(
      "a" -> Signal[Expr](Plus(Ref("b"), Literal(1))),
      "b" -> Signal[Expr](Minus(Ref("c"), Literal(3))),
      "c" -> Signal[Expr](Minus(Ref("a"), Literal(3)))
    )
    computeValues(input)
    //    println(hasCyclicDependencies("a", Ref("b"), input))
  }
}
