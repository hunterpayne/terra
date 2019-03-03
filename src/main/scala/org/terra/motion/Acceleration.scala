/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package motion

import time._
import mass.MassLike
import space.LengthLike

/**
 * Represents a quantity of acceleration
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class AccelerationLike[C <: TypeContext](
  val value: C#T, val unit: AccelerationUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[AccelerationLike[C], C#T, C]
    with TimeDerivative[VelocityLike[C], C#T, C#T, C]
    with SecondTimeDerivative[LengthLike[C], C]
    with TimeIntegral[JerkLike[C], C#T, C#T, C] {

  import ops.accelerationOps._
  import ops.velocityOps.MetersPerSecond
  import ops.jerkOps.MetersPerSecondCubed
  import ops.timeOps.Seconds
  import ops.forceOps.Newtons
  import ops.lengthOps.{ Feet, Millimeters, UsMiles }

  type Length = LengthLike[C]
  type Mass = MassLike[C]
  type Force = ForceLike[C]
  type Velocity = VelocityLike[C]

  def dimension: Dimension[AccelerationLike[C], C#T, C] = Acceleration

  protected[terra] def timeIntegrated =
    MetersPerSecond(toMetersPerSecondSquared)
  protected[terra] def timeDerived = 
    MetersPerSecondCubed(toMetersPerSecondSquared)
  protected[terra] def time = Seconds(1)

  def *(that: Mass)(implicit ops: TerraOps[C]): Force = {
    Newtons(ops.num.times(this.toMetersPerSecondSquared, that.toKilograms))
  }
  def *(that: TimeSquared)(implicit ops: TerraOps[C]): Length = 
    this * that.time1 * that.time2

  def toFeetPerSecondSquared = to(FeetPerSecondSquared)
  def toMillimetersPerSecondSquared = to(MillimetersPerSecondSquared)
  def toMetersPerSecondSquared = to(MetersPerSecondSquared)
  def toUsMilesPerHourSquared = to(UsMilesPerHourSquared)
  def toEarthGravities = to(EarthGravities)

  def analyze(distance: Length)(implicit ops: TerraOps[C]): (Time, Velocity) = {
    val timeToDistance = (distance * ops.convDouble(2) / this).squareRoot
    val finalVelocity = this * timeToDistance
    (timeToDistance, finalVelocity)
  }
  def analyze(accelerationTime: Time)(
    implicit ops: TerraOps[C]): (Length, Velocity) = {
    val finalVelocity = this * accelerationTime
    val distance = this * accelerationTime.squared * ops.convDouble(0.5)
    (distance, finalVelocity)
  }
  def analyze(velocity: Velocity)(implicit ops: TerraOps[C]): (Time, Length) = {
    val timeToVelocity = velocity / this
    val distance = this * timeToVelocity.squared * ops.convDouble(0.5)
    (timeToVelocity, distance)
  }
}

/**
 * Base trait for units of [[org.terra.motion.Acceleration]]
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 */
trait AccelerationUnit[C <: TypeContext] 
    extends UnitOfMeasure[AccelerationLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) =
    new AccelerationLike[C](t, this)
}

trait AccelerationOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait AccelerationUnitT extends AccelerationUnit[C]

  object Acceleration extends Dimension[AccelerationLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: AccelerationUnit[C])(
      implicit n: Numeric[A]) =
      new AccelerationLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Acceleration"
    def primaryUnit = MetersPerSecondSquared
    def siUnit = MetersPerSecondSquared
    def units = Set(FeetPerSecondSquared, MillimetersPerSecondSquared, MetersPerSecondSquared, UsMilesPerHourSquared,
      EarthGravities)
  }

  import ops.lengthOps.{ Millimeters, Meters, Feet, UsMiles }

  object MillimetersPerSecondSquared extends AccelerationUnitT with SiUnit {
    val symbol = "mm/s²"
    val conversionFactor = Millimeters.conversionFactor / Meters.conversionFactor
  }

  object MetersPerSecondSquared extends AccelerationUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "m/s²"
  }

  object FeetPerSecondSquared extends AccelerationUnitT {
    val symbol = "ft/s²"
    val conversionFactor = Feet.conversionFactor / Meters.conversionFactor
  }

  object UsMilesPerHourSquared extends AccelerationUnitT {
    val symbol = "mph²"
    val conversionFactor = 
      (UsMiles.conversionFactor / Meters.conversionFactor) / 
        math.pow(ops.timeOps.Time.SecondsPerHour, 2)
  }

  /**
    * Represents acceleration in Earth gravities also knows as G-Force, or g's
    */
  object EarthGravities extends AccelerationUnitT {
    val symbol = "g"
    val conversionFactor = 9.80665 * Meters.conversionFactor
  }

  object AccelerationConversions {

    implicit class AccelerationConversions[A](a: A)(implicit n: Numeric[A]) {
      def mpss = MetersPerSecondSquared(a)
      def fpss = FeetPerSecondSquared(a)
    }
  }
}
