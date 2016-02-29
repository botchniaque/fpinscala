import fpinscala.state.RNG

var r:RNG = RNG.Simple(5)

val (i, nr) = RNG.int(r)
RNG.int(nr)
RNG.nonNegativeInt(nr)
RNG.double(nr)
RNG.ints(4)(nr)
RNG._double(nr)

RNG.intsSeq(5)(nr)
