def from(n: Int): LazyList[Int] = n #:: from(n+1)

// will not evaluate
val nats = from(0)

// will not evaluate
nats.map(_ * 4)

// will not evaluate
nats.take(3)

// conversion toList will evaluate
nats.take(3).toList

// toList would evaluate to infinite list
// nats.toList

// head is also the terminal operation
nats.map(_ * 4).head