package org.mmg.scala.study

/**
 * Created by mmedina on 14/09/08.
 */
object PatternMatching {

  def basicCase(x: Int): String = x match {
    case 1 => "one"
    case 2 => "two"
    case _ => "I can't count after 2!!!"
  }

  def displayHelp = "This is the Scala help line. How may I help you?"
  def displayVersion = "Version 1.0"
  def unknownArgument(arg: String) = "I don't know the meaning of the argument %s".format(arg)

  def parseArgument(arg: String) = arg match {
    case "-h" | "--help" => displayHelp
    case "-v" | "--version" => displayVersion
    case whatever => unknownArgument(whatever)
  }

  def typedPatternMatching(x: Any): String = x match {
    case i:Int => "integer: " + i
    case _:Double => "a double"
    case s:String => "I want to say " + s
    case _ => "This is not good"
  }

  /*
   With Option
   This will fail if the option is None. Scala will warn us
    */
  def checkOption(o: Option[Int]): Int = o match {
    case Some(number) => number // My homework!!! Variable binding (what is happening here?)
    case _ => 0
  }

  // Length of a list

  def lengthOfList(l: List[Any]): Int = l match {
    case h :: t => 1 + lengthOfList(t)
    case Nil => 0 // What is Nil? (Object? Class?)
  }

  // Even better
  def betterLengthOfList(l: List[Any]): Int = l match {
    case _ :: t => 1 + lengthOfList(t)
    case _ => 0
  }

  /*
  We can check for lists in different ways
   */

  def checkMiddleWord(l: List[String]) = l match {
    case h :: "dog" :: t => "The second element is dog"
    case "He" :: _ :: "at" :: t => "Looks a like a sentence"
    case _ => "Anything else"
  }

  def checkList(l: List[Any]) = l match {
    case _ :: _ :: t => "The list has at least 2 elements"
    case _ :: Nil => "The list has 1 element"
    case _ => "Empty List"
  }


  // Using guards
  def lengthWithGuards(l: List[Any]): Int = l match {
    case l if l.nonEmpty => 1 + lengthWithGuards(l.tail)
    case _ => 0
  }

  def oddOrEven(number: Int) = number match {
    case 0 => "Zero, by definition, is even"
    case x if x % 2 == 0 => 3
    case _ => "odd"
  }

  // What happens with pattern matching?
  // Let's create a class

  class JIEMEmployee(val name: String, val dept: String) {
    def doNothing = println("I'm not doing anything")
  }

  class JIEMManager(name: String, val role: String) extends JIEMEmployee(name, "Bosses") {
    override def doNothing = println("I do a lot of work")
    private def doSomething = println("jojojo")
  }

  // companion object

  object JIEMEmployee {
    // apply allows to create a new member of the class without using new
    def apply(name: String, dept: String) = new JIEMEmployee(name, dept)

    // unapply returns an Option[Tuple] with the elements that were used when creating the class
    def unapply(jiememployee: JIEMEmployee) = Some((jiememployee.name, jiememployee.dept))
  }

  object JIEMManager {
    def apply(name: String, role: String) = new JIEMManager(name, role)
    def unapply(jiemManager: JIEMManager) = Some((jiemManager.name, jiemManager.dept, jiemManager.role))
  }

  case class AnEmployee(name: String, salary: Double)


  // Note the "sealed" keyword
  sealed trait Expression
  object Plus extends Expression
  object Minus extends Expression
  object Times extends Expression
  object Division extends Expression

  def evaluateExpression(e: Expression): String = e match {
    case Plus => "+"
    case Minus => "-"
    case Times => "*"
    case Division => "/"
  }

  def getDepartmentFromEmployee(employee: JIEMEmployee) = employee match {
    case JIEMManager(_,dept,role) => "Manager of %s with role %s".format(dept, role)
    case JIEMEmployee(_,dept) => dept
  }


  def main(a: Array[String]): Unit = {
    val Manuel = JIEMEmployee("Manuel", "Lab")
    val Mikko = JIEMEmployee("Mikko", "Lab")
    val Sano = JIEMManager("Sano", "System Boss") // No companion object

    List(Manuel,Mikko,Sano) foreach {employee => println(getDepartmentFromEmployee(employee))}

  }

}
