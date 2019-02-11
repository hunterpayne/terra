/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package motion

import scala.reflect.ClassTag

import time._
import mass.MassLike
import space.LengthLike

/**
 * Represents a quantify of Velocity
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class VelocityLike[C <: TypeContext](
  val value: C#T, val unit: VelocityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[VelocityLike[C], C#T, C]
    with TimeIntegral[AccelerationLike[C], C#T, C#T, C]
    with SecondTimeIntegral[JerkLike[C], C]
    with TimeDerivative[LengthLike[C], C#T, C#T, C] {

  import ops.velocityOps._
  import ops.timeOps.Seconds
  import ops.lengthOps.Meters
  import ops.accelerationOps.MetersPerSecondSquared
  import ops.momentumOps.NewtonSeconds

  type Mass = MassLike[C]
  type Jerk = JerkLike[C]
  type Momentum = MomentumLike[C]

  def dimension: Dimension[VelocityLike[C], C#T, C] = Velocity

  def timeDerived = MetersPerSecondSquared(toMetersPerSecond)
  protected[terra] def timeIntegrated = Meters(toMetersPerSecond)
  protected[terra] def time = Seconds(1)

  def *(that: Mass): Momentum = {
    implicit val opsArg = ops
    NewtonSeconds(ops.num.times(this.toMetersPerSecond, that.toKilograms))
  }

  def /(that: TimeSquared)(implicit ops: TerraOps[C]): Jerk =
    this / that.time1 / that.time2
  def /(that: Jerk)(implicit ops: TerraOps[C]): TimeSquared =
    (this / that.timeIntegrated) * this.time

  def toFeetPerSecond = to(FeetPerSecond)
  def toMillimetersPerSecond = to(MillimetersPerSecond)
  def toMetersPerSecond = to(MetersPerSecond)
  def toKilometersPerSecond = to(KilometersPerSecond)
  def toKilometersPerHour = to(KilometersPerHour)
  def toUsMilesPerHour = to(UsMilesPerHour)
  def toInternationalMilesPerHour = to(InternationalMilesPerHour)
  def toKnots = to(Knots)
}

trait VelocityUnit[C <: TypeContext] 
    extends UnitOfMeasure[VelocityLike[C], C#T, C] with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new VelocityLike[C](t, this)
}

trait VelocityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait VelocityUnitT extends VelocityUnit[C]

  object Velocity extends Dimension[VelocityLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: VelocityUnit[C])(
      implicit n: Numeric[A]) =
      new VelocityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(l: LengthLike[C], t: TimeLike[C]) = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      MetersPerSecond(ops.div[C#T](l.toMeters, ops.rconvT(t.toSeconds)))
    }
    def apply(value: Any) = parse(value)
    def name = "Velocity"
    def primaryUnit = MetersPerSecond
    def siUnit = MetersPerSecond
    def units = Set(MetersPerSecond, FeetPerSecond, MillimetersPerSecond, KilometersPerSecond, KilometersPerHour,
      UsMilesPerHour, InternationalMilesPerHour, Knots)
  }

  import ops.lengthOps.{ Feet, Meters, Millimeters, Kilometers, UsMiles, InternationalMiles, NauticalMiles }
  import ops.timeOps.Time.SecondsPerHour

  object FeetPerSecond extends VelocityUnitT {
    val symbol = "ft/s"
    val conversionFactor = Feet.conversionFactor / Meters.conversionFactor
  }

  object MillimetersPerSecond extends VelocityUnitT with SiUnit {
    val symbol = "mm/s"
    val conversionFactor = 
      Millimeters.conversionFactor / Meters.conversionFactor
  }

  object MetersPerSecond extends VelocityUnitT with PrimaryUnit[C#T, C] 
      with SiUnit {
    val symbol = "m/s"
  }

  object KilometersPerSecond extends VelocityUnitT with SiUnit {
    val symbol = "km/s"
    val conversionFactor = Kilometers.conversionFactor / Meters.conversionFactor
  }

  object KilometersPerHour extends VelocityUnitT {
    val symbol = "km/h"
    val conversionFactor = (Kilometers.conversionFactor / Meters.conversionFactor) / SecondsPerHour
  }

  object UsMilesPerHour extends VelocityUnitT {
    val symbol = "mph"
    val conversionFactor = 
      (UsMiles.conversionFactor / Meters.conversionFactor) / SecondsPerHour
  }

  object InternationalMilesPerHour extends VelocityUnitT {
    val symbol = "imph"
    val conversionFactor = 
      (InternationalMiles.conversionFactor / Meters.conversionFactor) / 
        SecondsPerHour
  }

  object Knots extends VelocityUnitT {
    val symbol = "kn"
    val conversionFactor = 
      (NauticalMiles.conversionFactor / Meters.conversionFactor) / 
        SecondsPerHour
  }

  object VelocityConversions {
    lazy val footPerSecond = FeetPerSecond(1)
    lazy val millimeterPerSecond = MillimetersPerSecond(1)
    lazy val meterPerSecond = MetersPerSecond(1)
    lazy val kilometerPerSecond = KilometersPerSecond(1)
    lazy val kilometerPerHour = KilometersPerHour(1)
    lazy val milePerHour = UsMilesPerHour(1)
    lazy val knot = Knots(1)

    implicit class VelocityConversions[A](a: A)(implicit n: Numeric[A]) {
      def fps = FeetPerSecond(a)
      def mps = MetersPerSecond(a)
      def kps = KilometersPerSecond(a)
      def kph = KilometersPerHour(a)
      def mph = UsMilesPerHour(a)
      def knots = Knots(a)
    }
  }
}

