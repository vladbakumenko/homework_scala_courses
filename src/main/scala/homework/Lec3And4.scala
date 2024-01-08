package homework

import homework.Lec3And4.{divideEither, divideOpt, divideTry}
import util.ImplicitForPrint

import scala.util.Try

class Book(name: String, author: String, year: Int) {
  override def toString: String = s"${this.getClass.getSimpleName}[name: $name, author: $author, year: $year]"
}

object Book {
  def createBook(name: String, author: String, year: Int): Book = {
    new Book(name, author, year)
  }
}

trait Shape {
  def getSquare: Double
}

case class Round(r: Double) extends Shape {
  override def getSquare: Double = {
    import scala.math.Pi
    r * r * Pi
  }
}

case class Square(x: Double) extends Shape {
  override def getSquare: Double = {
    x * x
  }
}

case class Rectangle(x: Double, y: Double) extends Shape {
  override def getSquare: Double = {
    x * y
  }
}

object Lec3And4 {
  def getSquareOfShape(shape: Shape): Double = {
    shape match {
      case rnd: Round => rnd.getSquare
      case sqr: Square => sqr.getSquare
      case rec: Rectangle => rec.getSquare
    }
  }

  def isContainStr(opt: Option[String], str: String): Unit = opt match {
    case Some(src) => if (src == str) println("Word found") else println("Word not found")
    case None => println("Source is null")
  }

  def howOldAreYou(opt: Option[Int]): Unit = opt.foreach {
    case age if age > 18 => println("adult")
    case age if 10 < age && age <= 18 => println("teenager")
    case age if age <= 10 && age > 0 => println("child")
    case _ => println("god")
  }

  def divideOpt(a: Int, b: Int): Option[Int] = b match {
    case 0 => None
    case b if b != 0 => Option(a/b)
  }

  def divideTry(a: Int, b: Int): Try[Int] = Try {
    a / b
  }

  def divideEither(a: Int, b: Int): Either[String, Int] =
    if (b != 0) Right(a/b)
    else Left("divide by zero with either")
}

object Main3And4 extends App with ImplicitForPrint {
  val book = Book.createBook("name", "author", 2000)
  book.print

  val rnd = Round(7)
  val sqr = Square(3.3)
  val rec = Rectangle(2, 10)

  List(rnd, sqr, rec).foreach(Lec3And4.getSquareOfShape(_).print)

  Lec3And4.isContainStr(Option("test"), "test")
  Lec3And4.isContainStr(Option("test"), "test2")
  Lec3And4.isContainStr(Option(null), "test2")

  List(11, 20, 1, -10).foreach(age => Lec3And4.howOldAreYou(Option(age)))

  divideOpt(10, 2).print
  divideOpt(10, 0).print

  divideTry(10, 2).getOrElse(0).print
  divideTry(10, 0).recover(_ => "divide by zero with try").foreach(_.print)

  divideEither(10, 0) match {
    case Left(er) => er.print
    case Right(value) => value.print
  }
}
