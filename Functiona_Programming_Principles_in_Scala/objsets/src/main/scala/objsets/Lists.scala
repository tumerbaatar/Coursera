package objsets

object Lists {
  def apply() = List()

  def apply(n: Int) = List(n)

  def apply(n1: Int, n2: Int) = List(n1, n2)

  def main(args: Array[String]): Unit = {
    val emptyList = Lists()
    val singletonList = Lists(1)
    val coupleList = Lists(1, 2)

    println(emptyList, singletonList, coupleList)
  }
}
