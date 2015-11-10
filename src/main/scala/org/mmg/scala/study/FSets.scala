package org.mmg.scala.study

object FSets {

  /*
  Wikipedia:
  数学において指示関数（しじかんすう、indicator function）、集合の定義関数あるいは
  特性関数（とくせいかんすう、characteristic function）は、
  集合の元がその集合の特定の部分集合に属するかどうかを指定することによって定義される関数である。

  I_A(x) =
      1 if x belongs to A,
      0 otherwise
   */

  // limit bounds just not to work with infinite values
  val limit = 1000

  type Set = Int => Boolean

  def contains(s: Set, elem: Int): Boolean = s(elem)

  def singletonSet(elem: Int): Set = {x => x == elem}

  def union(s: Set, t: Set): Set = {x => contains(s,x) || contains(t,x)}

  def intersect(s: Set, t:Set): Set = {x => contains(s,x) && contains(t,x)}

  def diff(s: Set, t: Set): Set = {x => contains(s,x) != contains(t,x)}

  def filter(s: Set, p: Int => Boolean): Set = {x => contains(s,x) && p(x)}

  def forall(s: Set, p: Int => Boolean): Boolean = {
    def iterate(a: Int): Boolean = {
      if (a > limit) true
      else if (contains(s,a)) p(a) && iterate(a + 1)
      else iterate(a + 1)
    }

    iterate(-limit)
  }

  def exist(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  def map(s: Set, f: Int => Int): Set = b => exist(s, c => f(c) == b)

  def toString(s: Set): String = {
    val setElements = for (i <- -limit to limit if contains(s, i)) yield i
    setElements.mkString("{",",", "}")
  }

  def printSet(s: Set): Unit = {
    println(toString(s))
  }

}
