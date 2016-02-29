object session {
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a+1, acc + f(a))
    }
    loop(a, 0)
  }

  sum((x)=> x)(1, 3)
  sum((a) => a)(1, 3)

  def product(f: Int => Int)(a :Int, b:Int): Int =
    if (a > b) 1
  else f(a) * product(f)(a + 1, b)

  product(x=> x)(3, 5)
  product2(x=> x)(3, 5)
  product(x=> x*x)(3, 4)
  product2(x=> x*x)(3, 4)

  def fact(n:Int) = product(x=>x)(1, n)

  fact(3)
  fact(5)

  def mapReduce(f:Int => Int, combine:(Int, Int)=> Int, zero: Int) (a: Int, b:Int):Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
  }

  def product2(f: Int => Int)(a :Int, b:Int): Int =
    mapReduce(f, (x, y) => x*y, 1)(a, b)


  

}