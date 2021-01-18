import rx.lang.scala._

val ticks: Observable[Long] = Observable.interval(1 seconds)
val evens: Observable[Long] = ticks.filter(s => s%2 == 0)

val bugs: Observable[Seq[Long]] = ticks.buffer(2, 1)
val s = bugs.subscribe(b=>println(b))

s.unsubscribe()