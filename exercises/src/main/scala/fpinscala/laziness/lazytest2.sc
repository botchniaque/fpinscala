import fpinscala.laziness

val s:laziness.Stream[Int] = fpinscala.laziness.Stream(1+1, 1+2, 1+3)
//val s:Stream[Int] = Stream(1+1, 1+2, 1+3)
s.toList
s.take(2).toList
s.take(1).toList
s.drop(2).toList
s.drop(1).toList

s.takeWhile(_ == 3).toList
s.takeWhile(_ >= 3).toList
s.takeWhile(_ <= 3).toList

s.takeWhile2(_ == 3).toList
s.takeWhile2(_ >= 3).toList
s.takeWhile2(_ <= 3).toList
s.forAll({print(1);_ - 2 >= 0})

s.headOption
Stream().headOption

s.map({print(1);_ + 1}).toList

s.filter(_%2==0).toList

s.append(fpinscala.laziness.Stream(1+4, 1+5)).toList

s.flatMap(a => fpinscala.laziness.Stream(a-1, a, a+1)).toList

val twos = fpinscala.laziness.Stream.constant("a")
twos.take(5).toList
fpinscala.laziness.Stream.from(4).take(5).toList
fpinscala.laziness.Stream.fibs.take(65).toList


