package homework

import com.sun.tools.javac.util.Assert
import homework.Lec5.DaysByEnum.Friday
import homework.Lec5.DaysByEnum.Monday
import homework.Lec5.DaysByEnum.Sunday
import homework.Lec5.DaysByEnum.Wednesday
import homework.Lec5.DaysByEnum.getDaysBetween
import homework.Lec5.DaysByEnum.getNextDay
import homework.Lec5.DaysBySealed
import homework.Lec5.getAverageFromOptInt
import homework.Lec5.getSumOfOptIntOrReturnNone
import homework.Lec5.getUniqElems
import homework.Lec5.DaysBySealed.Monday
import homework.Lec5.DaysBySealed.Tuesday
import util.UtilF.writeNumberOfTask

import java.math.MathContext
import scala.util.Try

object Lec5 extends {

  object DaysByEnum extends Enumeration {
    type Day = Value

    val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value

    def getDaysBetween(dayOne: Day, dayTwo: Day): Seq[Day] = {
      DaysByEnum.values.filter(day => day.id > dayOne.id && day.id < dayTwo.id).toSeq
    }

    def getNextDay(day: Day): Day = {
      day.id match {
        case id: Int if id == (DaysByEnum.values.size - 1) => DaysByEnum(0)
        case id: Int                                       => DaysByEnum(id + 1)
      }
    }
  }

  object DaysBySealed {
    sealed trait DayOfTheWeek

    case class Monday() extends DayOfTheWeek

    case class Tuesday() extends DayOfTheWeek

    case class Wednesday() extends DayOfTheWeek

    case class Thursday() extends DayOfTheWeek

    case class Friday() extends DayOfTheWeek

    case class Saturday() extends DayOfTheWeek

    case class Sunday() extends DayOfTheWeek

    def getNextDay(currDay: DayOfTheWeek): DayOfTheWeek = {
      currDay match {
        case Monday()    => Tuesday()
        case Tuesday()   => Wednesday()
        case Wednesday() => Thursday()
        case Thursday()  => Friday()
        case Friday()    => Saturday()
        case Saturday()  => Sunday()
        case Sunday()    => Monday()
      }
    }
  }

  def getUniqElems(seq: Seq[Any]) = seq.toSet

  def getAverageFromOptInt(seq: Seq[Option[Int]]): Double = {
    val withoutOpt = for {
      opt <- seq
      x   <- opt
    } yield x

    val sum    = withoutOpt.sum
    val length = withoutOpt.length

    BigDecimal.decimal(sum.toDouble / length).round(new MathContext(3)).toDouble
  }

  def getSumOfOptIntOrReturnNone(seq: Seq[Option[Int]]) = {
    if (!seq.forall(_.isDefined)) {
      Option.empty
    } else {
      seq.flatten.sum
    }
  }
}

object Main5 extends App {

  writeNumberOfTask(1)
  println(getDaysBetween(Friday, Sunday))

  writeNumberOfTask(2)
  println(getNextDay(Sunday))
  println(DaysBySealed.getNextDay(new Monday))

  writeNumberOfTask(3)
  val nums = Seq(1, 2, 2, 1, 3, 3, 4, 4, 5)
  println(getUniqElems(nums))
  val words = Seq("one", "one", "two", "three", "three")
  println(getUniqElems(words))

  writeNumberOfTask(4)
  val optNums = nums.map(Option(_)) ++ Seq(Option.empty[Int])
  println(getAverageFromOptInt(optNums))

  writeNumberOfTask(5)
  println(getSumOfOptIntOrReturnNone(optNums.filter(_.ne(None))))
  println(getSumOfOptIntOrReturnNone(optNums))


}
