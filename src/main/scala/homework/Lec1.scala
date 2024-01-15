package homework

import homework.Lec1.{Point, circleArea, distanceBetweenTwoPoints, fromCelsiusToFahrenheit, square, strLength}
import util.ImplicitForPrint
import util.UtilF.writeNumberOfTask

import scala.math._

object Lec1 {

  def square(x: Int): Int = {
    x * x
  }

  def circleArea(radius: Int): Double = {
    val pi = Pi
    pi * square(radius)
  }

  def fromCelsiusToFahrenheit(celsius: Double): Double = {
    val ratio = 9 / 5
    celsius * ratio + 32
  }

  val strLength: String => Int = (str: String) => str.length

  case class Point(x: Int, y: Int)

  def distanceBetweenTwoPoints(p1: Point, p2: Point): Double = {
    val difX = abs(p1.x - p2.x)
    val difY = abs(p1.y - p2.y)
    sqrt(square(difX) + square(difY))
  }
}

object Main1 extends App with ImplicitForPrint {

  writeNumberOfTask(1)
  List(2, 4, 8).foreach(x => (s"square of number $x is ${square(x)}").print)

  writeNumberOfTask(2)
  val radius = 10
  s"area of circle with radius $radius is ${circleArea(radius)}".print

  writeNumberOfTask(3)
  val degree = 30.5
  val req = s"$degree degree of celsius in fahrenheit is ${fromCelsiusToFahrenheit(degree)}"
  req.print

  writeNumberOfTask(4)
  s"length of string req is ${strLength(req)}".print

  writeNumberOfTask(5)
  val p1 = Point(2, 9)
  val p2 = Point(10, 7)
  s"distance between $p1 and $p2 is ${distanceBetweenTwoPoints(p1, p2)}".print
}
