package util

object UtilF {
  def writeNumberOfTask(num: Int): Unit = {
    println("\n" + s"------- task ${num} -------")
  }
}

trait ImplicitForPrint {
  implicit class PrintMe[T <: Any](any: T) {
    /** just println(any) */
    implicit def print: Unit = println(any)
  }
}
