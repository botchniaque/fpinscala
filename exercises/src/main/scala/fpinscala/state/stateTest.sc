import fpinscala.state._

var r:RNG = RNG.Simple(5)

val (i, nr) = RNG.int(r)
RNG.int(nr)
RNG.nonNegativeInt(nr)
RNG.double(nr)
RNG.ints(4)(nr)
RNG._double(nr)
RNG.intsSeq(5)(nr)
RNG.nonNegativeLessThan(436)(nr)
val inputs = List(Coin, Turn)
State.simulateMachine(inputs).run(new Machine(true, 5, 10))