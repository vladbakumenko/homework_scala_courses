package homework

import homework.Lec2.{defineNumber, evenOrOdd, fibonacciTailRecurcive, fibonnacсi}
import util.ImplicitForPrint
import util.UtilF.writeNumberOfTask

object Lec2 {

  def evenOrOdd(num: Int): String = {
    val str = s"number $num is "
    if (num % 2 == 0) str ++ "even" else str ++ "odd"
  }

  def defineNumber(num: Double): String = {
    if (num > 0) {
      s"number $num is positive"
    }
    else if (num < 0) {
      s"number $num is negative"
    }
    else s"number $num is zero"
  }

  def fibonnacсi(num: Int): Int = {
    if (num == 0) 0
    else if (num == 1) 1
    else {
      fibonnacсi(num - 2) + fibonnacсi(num - 1)
    }
  }

  def fibonacciTailRecurcive(num: Int): Int = {
    def fibHelper(num: Int, acc1: Int, acc2: Int): Int = num match {
      case 0 => acc1
      case 1 => acc2
      case _ => fibHelper(num - 1, acc2, acc1 + acc2)
    }

    fibHelper(num, 0, 1)
  }

}

object Main2 extends App with ImplicitForPrint {

  writeNumberOfTask(1)
  List(1, 8, 0).foreach(evenOrOdd(_).print)

  writeNumberOfTask(2)
  List(1, -8, 0).foreach(defineNumber(_).print)

  writeNumberOfTask(3)
  val l1 = for (i <- 1 to 10) yield i
  l1.print
  val l2 = for (i <- 1 until 10) yield i
  l2.print

  writeNumberOfTask(4)
  val l3 = for (i <- 1 to 10) yield 5 * i
  l3.print

  writeNumberOfTask(5)
  for (i <- 1 to 10) fibonnacсi(i).print
  for (i <- 1 to 10) fibonacciTailRecurcive(i).print
}
