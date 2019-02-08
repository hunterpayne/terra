
package org.terra
package time

import scala.concurrent.duration.{ DAYS, Duration, HOURS, MICROSECONDS, MILLISECONDS, MINUTES, NANOSECONDS, SECONDS }

/**
  * Contains all the symbols exported by the org.terra.*.time packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * @author Hunter Payne
  */
trait TimeSymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type Time = TimeLike[Tuple]
  lazy val Nanoseconds = ops.timeOps.Nanoseconds
  lazy val Microseconds = ops.timeOps.Microseconds
  lazy val Milliseconds = ops.timeOps.Milliseconds
  lazy val Seconds = ops.timeOps.Seconds
  lazy val Minutes = ops.timeOps.Minutes
  lazy val Hours = ops.timeOps.Hours
  lazy val Days = ops.timeOps.Days
  lazy val Time = ops.timeOps.Time

  object TimeConversions {

    import ops.timeOps.{ TimeConversions => Convs }

    lazy val nanosecond = Convs.nanosecond
    lazy val microsecond = Convs.microsecond
    lazy val millisecond = Convs.millisecond
    lazy val second = Convs.second
    lazy val minute = Convs.minute
    lazy val halfHour = Convs.halfHour
    lazy val hour = Convs.hour
    lazy val day = Convs.day

    implicit class TimeConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.TimeConversions[A](a)

    implicit class TimeStringConversions(s: String)
        extends Convs.TimeStringConversions(s)

    implicit object TimeNumeric
        extends AbstractQuantityNumericT[TimeLike[Tuple], Tuple](Time)

    implicit def timeToScalaDuration(time: TimeLike[Tuple]): Duration =
      Convs.timeToScalaDuration(time)

    implicit def scalaDurationToTime(duration: Duration)(
      implicit ops: TerraOps[Tuple]): TimeLike[Tuple] =
      Convs.scalaDurationToTime(duration)
  }

  type TimeSquared = TimeSquaredLike[Tuple]
  lazy val SecondsSquared = ops.timeSquaredOps.SecondsSquared
  lazy val MinutesSquared = ops.timeSquaredOps.MinutesSquared
  lazy val HoursSquared  = ops.timeSquaredOps.HoursSquared
  lazy val TimeSquared = ops.timeSquaredOps.TimeSquared

  type Frequency = FrequencyLike[Tuple]
  lazy val Hertz = ops.frequencyOps.Hertz
  lazy val Kilohertz = ops.frequencyOps.Kilohertz
  lazy val Megahertz = ops.frequencyOps.Megahertz
  lazy val Gigahertz = ops.frequencyOps.Gigahertz
  lazy val Terahertz = ops.frequencyOps.Terahertz
  lazy val RevolutionsPerMinute = ops.frequencyOps.RevolutionsPerMinute
  lazy val Frequency = ops.frequencyOps.Frequency

  object FrequencyConversions {

    import ops.frequencyOps.{ FrequencyConversions => Convs }

    implicit class FrequencyConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.FrequencyConversions[A](a)

    implicit object FrequencyNumeric
        extends AbstractQuantityNumeric[FrequencyLike[Tuple], Tuple](Frequency)
  }
}
