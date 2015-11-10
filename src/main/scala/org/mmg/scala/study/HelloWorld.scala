package org.mmg.scala.study
/**
 * Created by mmedina on 14/08/11.
 */
object HelloWorld {

  def sayHello(hellomsg: String): Unit = {
    println(hellomsg)
  }

}

object TheMainObject {

  def imperativeIterations = {
    var i = 1
    while (i < 10) {
      println("i is %d".format(i))
      i +=1
    }

    for (i <- 1 to 10) {
      println("i in for is: %d".format(i))
    }

    i = 0
    do {
      println("i in do is: %d".format(i))
      i += 1
    } while (i < 10)

  }

  def functionalIterations = {
    (1 to 10).foreach(i => println("i in functional iterations is: %d".format(i)))

    (1 to 10).foreach(println)

    1 to 10 foreach println

    1.to(10)

    // Infinite sequence lazily evaluated
    Stream.from(1) take 10 foreach println
  }

  def loop: Boolean = loop

  def and(x: Boolean, y: Boolean): Boolean =
    if (x) y else false


  def betterAnd(x: Boolean, y: => Boolean): Boolean =
    if (x) y else false

  val sayHello = (msg: String) => println(msg)
  def main(a: Array[String]): Unit = {
    betterAnd(false, loop)
    HelloWorld.sayHello("Hello!")
    sayHello("Hello from val!")
    imperativeIterations
    functionalIterations
  }
}
