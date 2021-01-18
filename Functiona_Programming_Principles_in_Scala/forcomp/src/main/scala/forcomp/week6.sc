val letters = List(List('a'), List('b'), List('c'), List('d'))

def shuffles(letters: List[List[Char]]): List[List[Char]] = {
  def combine(letters: List[List[Char]], prevLetters: List[List[Char]]): List[List[Char]] = {
    if (prevLetters == letters) {
      letters
    } else {
      (for {
        l <- combine(letters, prevLetters)
        t <- letters
        if !l.contains(t.head)
      } yield (l ++ t).sorted)
        .distinct
        .sortBy((i: List[Char]) => i.toString)
    }

  }

  combine(letters, letters)
}

shuffles(letters)
println("aaaa")

//val pairs = (for {
//  l <- letters
//  t <- letters
//  if !l.contains(t.head)
//} yield (l ++ t).sorted)
//  .distinct.sortBy((i: List[Char]) => i.toString)
//
//val triples = (for {
//  p <- pairs
//  l <- letters
//  if !p.contains(l.head)
//} yield (l ++ p).sorted)
//  .distinct.sortBy((i: List[Char]) => i.toString)
//
//val quads = (for {
//  t <- triples
//  l <- letters
//  if !t.contains(l.head)
//} yield (l ++ t).sorted)
//  .distinct.sortBy((i: List[Char]) => i.toString)
//
//
//val s = letters ++ pairs ++ triples ++ quads
//
//println(s)
//println("azaza")
