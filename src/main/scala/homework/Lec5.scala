package homework

import homework.Lec5.DaysByEnum.Friday
import homework.Lec5.DaysByEnum.Sunday
import homework.Lec5.DaysByEnum.Wednesday
import homework.Lec5.DaysByEnum.getDaysBetween
import homework.Lec5.DaysByEnum.getNextDay
import util.UtilF.writeNumberOfTask

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
    sealed trait Days {
      val index: Int
    }

    case class Monday(index: Int)    extends Days
    case class Tuesday(index: Int)   extends Days
    case class Wednesday(index: Int) extends Days
    case class Thursday(index: Int)  extends Days
    case class Friday(index: Int)    extends Days
    case class Saturday(index: Int)  extends Days
    case class Sunday(index: Int)    extends Days
  }

}

object Main5 extends App {

  writeNumberOfTask(1)
  println(getDaysBetween(Friday, Sunday))

  writeNumberOfTask(2)
  println(getNextDay(Sunday))


}
