package org.mmg.scala.study

/**
 * Created by mmedina on 14/08/29.
 */
object Collections {

  val l: List[Int] = List(1,2,3,4)
  val seq = Seq(1,2,3,4)
  val m = Map("a"-> 1, "b" -> 2, "c" -> 3)
  val o = Some(3) // ???

  val plusOne = (x: Int) => x + 1

  /*
   map: (a => b) => f a => f b
   Here, types a and b are both Int
   In Scala, the declaration (for lists) is:
   map[B](f: (A) ⇒ B): List[B]
    */

 //

  def mappingList = l.map(plusOne)

  def mappingSeq = seq.map(plusOne)

  // Each element is yielded as tuple (String, Int)
  def mappingMap = m.map(kv => kv._1 -> plusOne(kv._2))

  def anotherMap = m map {case (key, value) => key -> plusOne(value)}



  // Now let's make a = Int and b = String

  val buildMessage = (x: Int) => "Your input is %d".format(x)


  /* Why to use "f" to denote a collection?
     Concept of "Functor"
     In Japanese: 関手
   */

  // ********************** To explain on Sep 09 ***************************
  def mappingOption = o.map(_ + 1) // What's that _ ?
  def xxx = o.map{_ + 1}
  def xy = o map println
  def yyy = o.map{x => println(x)}


  /*
   flatMap: (a => f b) => f a => f b
   */

  val buildListOfMessages = (x: Int) => List("Input in list: %d".format(x))
  val eyuwieyuwieu = (x: Int) => "JOJOJO"

  // If we apply map here, we'll get a List[List[String]]

  def justMappingList = l map buildListOfMessages // Point-free style

  // With flatMap, we get a List[String]. flatMap = map + flatten

  def flatMappingList = l flatMap buildListOfMessages

  // Why is flatMap useful? Example with Options

  val salaryByCompany = Map("JIEM" -> 1.98, "Fishy Company" -> 1000000.00000)

  def xyz = salaryByCompany map { case (company, salary) => salary + 3.0 }

  val bonusByManager = Map("Sano" -> 100000.0, "Bill Gates" -> 1.0)

  def getCompanyForEmployee(employee: String): Option[String] = employee match {
    case "Manuel" => Some("JIEM")
    case "Alice" => Some("Fishy Company")
    case _ => None
  }

  def getGeneralManagerForCompany(company: String): Option[String] = company match {
    case "JIEM" => Some("Sano")
    case "Fishy Company" => Some("Bill Gates")
    case _ => None
  }

  def getBonusFromManager(manager: String): Option[Double] = bonusByManager.get(manager)

  // How can we get Manuel's bonus?
  // 1. The old way
  def getBonusForEmployee(employee: String): Double = {
    val company = getCompanyForEmployee(employee) getOrElse "Unknown"
    val manager = getGeneralManagerForCompany(company) getOrElse "Unknown"
    getBonusFromManager(manager).getOrElse(0)
  }

  // 2. With explicit flatMap
  def getBonusForEmployeeWithFlatMap(employee: String): Double = {
    getCompanyForEmployee(employee).flatMap{ company =>
       getGeneralManagerForCompany(company).flatMap{manager =>
         getBonusFromManager(manager)
       }
    }.getOrElse(0)
  }

  // 3. With for comprehensions
  def getBonusForEmployeeWithForComprehensions(employee: String): Double = {
    val maybeBonus = for {
      company <- getCompanyForEmployee(employee)
      manager <- getGeneralManagerForCompany(company)
      bonus <- getBonusFromManager(manager)
    } yield bonus

    maybeBonus getOrElse 0
  }

  def getSalaryFromCompany(company: String): Option[Double] = salaryByCompany.get(company)

  // How can we get Manuel's salary?
  // 1. The old way
  def getSalaryForEmployee1(employee: String): Double = {
    val companyForManuel = getCompanyForEmployee(employee) getOrElse "Unknown"
    getSalaryFromCompany(companyForManuel) getOrElse 0.0
  }

  // 2. With flatMap

  def getSalaryForEmployee2(employee: String): Double = {
    getCompanyForEmployee(employee).flatMap(company => getSalaryFromCompany(company)).getOrElse(0.0)
  }

  // 3. With for comprehensions
  def getSalaryForEmployee3(employee: String): Double = {
    val maybeSalary = for {
      company <- getCompanyForEmployee(employee)
      salary <- getSalaryFromCompany(company)
    } yield salary

    maybeSalary getOrElse 0.0
  }


  /*
  Defining flatMap in terms of Category Theory (or sets) is a little bit more complicated.
  Not touched now.

   */

  /* Working with Option
  A "container". It can contains something or nothing at all.
   */

  def badDivision (x: Int, y: Int) = x / y

  def slightlyBetterDivision(x: Int, y: Int) = {
    if (y == 0)
      throw new Exception("y is zero")
    else
      x.toDouble / y
  }

  // Using option

  def betterDivision(x: Int, y: Int): Option[Double] = if (y == 0) None else Some(x.toDouble/y)

  // How to check the result? Pattern matching!

  def doSomething(x: Int, y: Int) = {
    val r1 = betterDivision(x,y)

    r1 match {
      case Some(result) => "The result of the division is %f".format(result)
      case None => "Y is zero"
    }
  }

  def doSomethingElse(x: Int, y: Int) = {
    betterDivision(x,y) getOrElse 0
  }

  // Some.map is Some(something). None.map is None
  def doEvenMore(x: Int, y: Int) = {
    betterDivision(x,y) map {result => "The result of the division is %f".format(result)} getOrElse "Y is Zero"
  }

  /* List.head returns an error if it's empty
     List.headOption returns an Option[A]
   */

  def listHead = {
    val h = l.head // OK

    val localList = List()

    val notOK = l.head // Error

    val OK = l.headOption // Nice!
  }
}
