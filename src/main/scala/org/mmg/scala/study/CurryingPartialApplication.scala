package org.mmg.scala.study

import java.io.FileWriter

/**
 * Created by mmedina on 14/09/29.
 */
object CurryingPartialApplication {

  /*
  Currying refers to take a function with arity n,
  and return a function with arity m, where m < n.

  There are some ways to curry a function in Scala.
   */

  // This is a "normal" function
  // (Int, Int) => Boolean
  def areEqual(x: Int, y: Int) = x == y



  // These are some methods to curry it.
  // 1. Explicitly return a function
  // Int => (Int => Boolean)
  def areEqual2(x: Int) = (y: Int) => x == y


  // 2. Define another list of parameters
  // Int => (Int => Boolean)
  def areEqual3(x: Int)(y: Int) = x == y

  // 3. use the curried method
  val fCurried = (areEqual _).curried

  /*
   カリー化をすることにより、
   ハイオーダーファンクションの適用が簡単になる。

   部分適用

   ファンクションが必要としているパラメータの数より
   少ないパラメータで呼び出すこと。
   戻り値は、残りのパラメータを受け取り、戻り値を
   返すファンクション.
   */

  val isEqualTo2 = (areEqual _).curried(2)

  val isEqualTo3 = areEqual3(3) _

  // You can do this as well
  val isEqualTo4 = areEqual(4,_:Int)

  def writeToLog(logFile: String)(msg: String) = {
    val fw = new FileWriter(logFile)
    fw.write(msg)
    fw.close
  }

  // これで３つのログファイルに簡単に書き込むことができる。
  val writeToAccessLog = writeToLog("access.log") _
  val writeToErrorLog = writeToLog("error.log") _
  val writeToProcessLog = writeToLog("process.log") _

   /*
   つまり、幾つかのパラメータを固定して、残りのパラメータを
   貰うファンクションを作成する。
    */

  /*
  逆もできる。アンカリー化
  ファンクションをとって、パラメータをグループ化して、
  すべてのパラメータと受け取るファンクションを返す。
   */

  def a3(x: Int)(y: Int)(z: Int) = x + y + z

  // flip Haskell

  val a3Uncurried = Function.uncurried(a3 _)

  /*
  All these principles sound boring and useless for some,
  but are very important as they are widely used in more
  advanced concepts. Example: State Monad.
  Do not worry about what a monad is right now, just bear
  in mind that, for convenience, the State Monad takes two generics
  and can be partially applied.
   */

  def mikkoFunction(f: Int => String)(x: Int)(msg: String) = {
    f(x)
    msg
  }

  val m1 = mikkoFunction((x: Int) => x.toString) _

  def main(a: Array[String]): Unit = {
    println("areEqual3: " + areEqual3(3)(2))
    println("isEqualTo3: " + isEqualTo3(2))
    println("fCurried: " + fCurried(3)(2))
    println("a3: " + a3(1)(2)(3))
    println("a3Uncurried: " + a3Uncurried(1,2,3))
    println("isEqualTo4: " + isEqualTo4(4))

    // This is the same as (areEqual _).curried(3)(2)
    println("Currying directly and applying: " + (areEqual _).curried.apply(3).apply(2))

    // Writing to logs

    // Same as writeToLog("access.log")("Access: Manuel accessed here now!")
    writeToAccessLog("Access: Manuel accessed here now!")
    writeToProcessLog("Processing stuff...")
    writeToErrorLog("Error: User not found")
  }

}