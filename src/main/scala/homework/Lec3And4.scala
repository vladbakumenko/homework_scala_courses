package homework

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
    case _ => println("Source is null")
  }

  def howOldAreYou(opt: Option[Int]): Unit = {
    opt.foreach {
      case age if age > 18 => println("adult")
      case age if 10 < age && age <= 18 => println("teenager")
      case age if age <= 10 && age > 0 => println("child")
      case _ => println("god")
    }
  }

  def divideTry(a: Int, b: Int): Try[Int] = Try {
    a / b
  }

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
}
