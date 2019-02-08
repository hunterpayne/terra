/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package time

import scala.reflect.ClassTag

import space.AreaLike
import radio.AreaTimeLike

import scala.concurrent.duration.{ DAYS, Duration, HOURS, MICROSECONDS, MILLISECONDS, MINUTES, NANOSECONDS, SECONDS }
import scala.language.implicitConversions

/**
  * Represents a quantity of Time
  *
  * @author  garyKeorkunian
  * @since   0.1
  *
  * @param value value in [[org.terra.time.Milliseconds]]
  */
final class TimeLike[C <: TypeContext](val value: C#TT, val unit: TimeUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[TimeLike[C], C#TT, C] {

  import ops.timeOps._
  import ops.areaTimeOps.SquareMeterSeconds

  type Area = AreaLike[C]
  type AreaTime = AreaTimeLike[C]
  type TimeSquared = TimeSquaredLike[C]
  type Time = TimeLike[C]

  def dimension: Dimension[TimeLike[C], C#TT, C] = Time

  override def getNumeric: Numeric[C#TT] = ops.numT
  override def getTag: ClassTag[C#TT] = ops.getClassTagTT
  override def makeEnsureType(implicit ops: TerraOps[C]): HasEnsureType[C#TT] =
    ops.converters.ensureTT
  override def makeFromCurrencyConverter(
    implicit ops: TerraOps[C]): HasConverter[C#TC, C#TT] =
    ops.converters.tcttConverter
  override def makeToCurrencyConverter(
    implicit ops: TerraOps[C]): HasConverter[C#TT, C#TC] =
    ops.converters.tttcConverter.asInstanceOf[HasConverter[C#TT, C#TC]]

  def millis = ops.nt[C#TT].toLong(toMilliseconds)

  def *[A <: org.terra.Quantity[A, TI, C] with TimeIntegral[_, TI, T, C], T, TI, C <: TypeContext](
    that: TimeDerivative[A, T, TI, C])(implicit ops: TerraOps[C]): A =
    that * this.asInstanceOf[TimeLike[C]]
  def *(that: FrequencyLike[C])(
    implicit ops: TerraOps[C]): DimensionlessLike[C] =
    new DimensionlessLike[C](
      ops.num.times(that.toHertz, ops.rconvT(toSeconds)),
      ops.dimensionlessOps.Each)

  def *(that: Time)(implicit ops: TerraOps[C]): TimeSquared =
    new TimeSquared(this, that)
  def squared = new TimeSquared(this, this)
  def *(that: Area)(implicit ops: TerraOps[C]): AreaTime = {
    implicit val tag = ops.getClassTagT
    SquareMeterSeconds(ops.num.times(
      ops.rconvT(this.toSeconds), that.toSquareMeters))
  }

  def toNanoseconds = to(Nanoseconds)
  def toMicroseconds = to(Microseconds)
  def toMilliseconds = to(Milliseconds)
  def toSeconds = to(Seconds)
  def toMinutes = to(Minutes)
  def toHours = to(Hours)
  def toDays = to(Days)
}

trait TimeUnit[C <: TypeContext] 
    extends UnitOfMeasure[TimeLike[C], C#TT, C] with UnitConverter[C#TT, C] {
  def apply(t: C#TT)(implicit tag: ClassTag[C#TT], ops: TerraOps[C]) =
    new TimeLike[C](t, this)
  override def apply[A](a: A)(implicit num: Numeric[A], ops: TerraOps[C]) = {
    implicit val tag = getTag
    new TimeLike[C](ops.convTime(num.toDouble(a)), this)
  }

  override private[terra] def makeDoubleConverter(
    implicit ops: TerraOps[C]): HasConverter[Double, C#TT] =
    ops.converters.dttConverter.asInstanceOf[HasConverter[Double, C#TT]]
  override private[terra] def makeLongConverter(
    implicit ops: TerraOps[C]): HasConverter[Long, C#TT] = 
    ops.converters.lttConverter.asInstanceOf[HasConverter[Long, C#TT]]

  override def makeEnsureType(implicit ops: TerraOps[C]): HasEnsureType[C#TT] =
    ops.converters.ensureTT
  override def getTag(implicit ops: TerraOps[C]): ClassTag[C#TT] =
    ops.getClassTagTT
}

trait TimeOps[C <: TypeContext] {

  implicit val numT: Numeric[C#TT]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait TimeUnitT extends TimeUnit[C]

  object Time extends Dimension[TimeLike[C], C#TT, C] with BaseDimension {
    val NanosecondsPerSecond = 1.0e9
    val MicrosecondsPerSecond = 1.0e6
    val MillisecondsPerNanosecond = 1.0e-6
    val MillisecondsPerMicrosecond = 1.0e-3
    val MillisecondsPerSecond = 1e3
    val MillisecondsPerMinute = MillisecondsPerSecond * 60d
    val MillisecondsPerHour = MillisecondsPerMinute * 60d
    val MillisecondsPerDay = MillisecondsPerHour * 24d
    val SecondsPerMinute = 60d
    val SecondsPerHour = SecondsPerMinute * 60d
    val SecondsPerDay = SecondsPerHour * 24
    val MinutesPerHour = 60d
    val HoursPerDay = 24d

    private[time] def apply[A](a: A, unit: TimeUnit[C])(
      implicit na: Numeric[A], ops: TerraOps[C]) =
      new TimeLike[C](ops.convTime(na.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def apply(duration: Duration): TimeLike[C] = duration.unit match {
      case NANOSECONDS  ⇒ Nanoseconds(duration.length)
      case MICROSECONDS ⇒ Microseconds(duration.length)
      case MILLISECONDS ⇒ Milliseconds(duration.length)
      case SECONDS      ⇒ Seconds(duration.length)
      case MINUTES      ⇒ Minutes(duration.length)
      case HOURS        ⇒ Hours(duration.length)
      case DAYS         ⇒ Days(duration.length)
    }

    def name = "Time"
    def primaryUnit = Milliseconds
    def siUnit = Seconds
    def units = Set(Nanoseconds, Microseconds, Milliseconds, Seconds, Minutes, Hours, Days)
    def dimensionSymbol = "T"
  }

  object Nanoseconds extends TimeUnitT with SiUnit {
    val conversionFactor = Milliseconds.conversionFactor / Time.MicrosecondsPerSecond
    val symbol = "ns"
  }

  object Microseconds extends TimeUnitT with SiUnit {
    val conversionFactor = Milliseconds.conversionFactor / Time.MillisecondsPerSecond
    val symbol = "µs"
  }

  object Milliseconds extends TimeUnitT with PrimaryUnit[C#TT, C] with SiUnit {
    val symbol = "ms"
  }

  object Seconds extends TimeUnitT with SiBaseUnit {
    val conversionFactor = Milliseconds.conversionFactor * Time.MillisecondsPerSecond
    val symbol = "s"
  }

  object Minutes extends TimeUnitT {
    val conversionFactor = Seconds.conversionFactor * Time.SecondsPerMinute
    val symbol = "m"
  }

  object Hours extends TimeUnitT {
    val conversionFactor = Minutes.conversionFactor * Time.MinutesPerHour
    val symbol = "h"
  }

  object Days extends TimeUnitT {
    val conversionFactor = Hours.conversionFactor * Time.HoursPerDay
    val symbol = "d"
  }

  object TimeConversions {
    lazy val nanosecond = Nanoseconds(1)
    lazy val microsecond = Microseconds(1)
    lazy val millisecond = Milliseconds(1)
    lazy val second = Seconds(1)
    lazy val minute = Minutes(1)
    lazy val halfHour = Minutes(30)
    lazy val hour = Hours(1)
    lazy val day = Days(1)

    implicit class TimeConversions[A](a: A)(implicit num: Numeric[A]) {
      def nanoseconds = Nanoseconds(a)
      def microseconds = Microseconds(a)
      def milliseconds = Milliseconds(a)
      def seconds = Seconds(a)
      def minutes = Minutes(a)
      def hours = Hours(a)
      def days = Days(a)
    }

    implicit class TimeStringConversions(s: String) {
      def toTime = Time(s)
    }


    /**
      * Converts a org.terra Time to Scala Duration
      *
      * NOTE - Because Scala Durations require a Long, the org.terra Time value most be converted / rounded
      *
      * @param time
      * @return
      */
    implicit def timeToScalaDuration(time: TimeLike[C]): Duration = time.unit match {
      case Nanoseconds  ⇒ Duration(numT.toLong(time.value), NANOSECONDS)
      case Microseconds ⇒ Duration(numT.toLong(time.value), MICROSECONDS)
      case Milliseconds ⇒ Duration(numT.toLong(time.value), MILLISECONDS)
      case Seconds      ⇒ Duration(numT.toLong(time.value), SECONDS)
      case Minutes      ⇒ Duration(numT.toLong(time.value), MINUTES)
      case Hours        ⇒ Duration(numT.toLong(time.value), HOURS)
      case Days         ⇒ Duration(numT.toLong(time.value), DAYS)
    }

    implicit def scalaDurationToTime(duration: Duration)(
      implicit ops: TerraOps[C]): TimeLike[C] =
      Time(duration)
  }
}

