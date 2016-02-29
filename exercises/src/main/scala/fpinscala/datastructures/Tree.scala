package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def size[A](t: Tree[A]): Int = {
    def go(size: Int, tree: Tree[A]): Int = tree match {
      case Leaf(_) => size + 1
      case Branch(l: Tree[A], r: Tree[A]) => size + 1 + go(size, l) + go(size, r)
    }
    go(0, t)
  }

  def max(t: Tree[Int]): Int = {
    def go(m: Int, curr: Tree[Int]): Int = curr match {
      case Leaf(x: Int) => m max x
      case Branch(l: Tree[Int], r: Tree[Int]) => go(m, l) max go(m, r)
    }
    go(Int.MinValue, t)
  }

  def depth[A](t: Tree[A]): Int = {
    def go(d: Int, curr: Tree[A]): Int = curr match {
      case Leaf(x: Int) => d
      case Branch(l: Tree[A], r: Tree[A]) => go(d + 1, r) max go(d + 1, l)
    }
    go(0, t)
  }

  def map[A, CC](t: Tree[A], f: A => CC): Tree[CC] = t match {
    case Leaf(a: A) => Leaf(f(a))
    case Branch(l: Tree[A], r: Tree[A]) => Branch(map(l, f), map(r, f))
  }

  def fold[A, B](t: Tree[A], z: B)(g: (B, B) => B)(f: (A, B) => B): B = t match {
    case Leaf(x: A) => f(x, z)
    case Branch(l: Tree[A], r: Tree[A]) => g(fold(l, z)(g)(f), fold(r, z)(g)(f))
  }

  def size2[A](t: Tree[A]): Int = {
    fold(t, 0)((l1:Int, l2) => l1 + l2 + 1)((_, a)=> a+1)
  }

  def depth2[A](t: Tree[A]): Int = {
    fold(t, 0)((l1:Int, l2) => (l1 max l2) + 1)((_, a)=> a)
  }

  def max2(t: Tree[Int]): Int = {
    fold(t, Int.MinValue)((l1:Int, l2) => l1 max l2)((v, a)=> a max v)
  }

  def map2[A, B](t: Tree[A], f: A => B): Tree[B] = {
    fold(t, null:Tree[B])(Branch(_, _))((v, _) => Leaf(f(v)))
  }


}


object TestTree {
  def main(args: Array[String]) {
    val t = Leaf(2)

    println(Tree.size(Leaf(2)))
    println("complex")
    val complex = Branch(Leaf(52), Branch(Branch(Leaf(2), Branch(Leaf(12), Leaf(5))), Leaf(4)))
    println(complex)
    println("Size:\t" + Tree.size(complex))
    println("Size2:\t" + Tree.size2(complex))
    println("Depth:\t" +Tree.depth(complex))
    println("Depth2:\t" +Tree.depth2(complex))
    println("Max:\t" +Tree.max(complex))
    println("Max2:\t" +Tree.max2(complex))
    println("Map:\t" +Tree.map(complex, (x:Int) => x * x))
    println("Map2:\t" +Tree.map2(complex, (x:Int) => x * x))
  }

}