
def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (x, z) => f(x) :: z )

mapFun(List(1, 2, 3), (x: Int) => x * x)

def lengthFun[T](xs: List[T]): Int =
  xs.foldRight(0)((_, z) => z + 1)

lengthFun(List(1, 2, 3, 4, 5))