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

import org.terra.time.{ SecondTimeDerivative, TimeDerivative, TimeSquaredLike }

/**
 * Represents the third time derivative of position after Velocity and Acceleration
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class JerkLike[C <: TypeContext](val value: C#T, val unit: JerkUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[JerkLike[C], C#T, C]
    with TimeDerivative[AccelerationLike[C], C#T, C#T, C]
    with SecondTimeDerivative[VelocityLike[C], C] {

  import ops.jerkOps._
  import ops.timeOps.Seconds
  import ops.accelerationOps.MetersPerSecondSquared

  type Velocity = VelocityLike[C]

  def dimension: Dimension[JerkLike[C], C#T, C] = Jerk

  protected[terra] def timeIntegrated =
    MetersPerSecondSquared(toMetersPerSecondCubed)
  protected[terra] def time = Seconds(1)

  def *(that: TimeSquared)(implicit ops: TerraOps[C]): Velocity =
    this * that.time1 * that.time2

  def toMetersPerSecondCubed = to(MetersPerSecondCubed)
  def toFeetPerSecondCubed = to(FeetPerSecondCubed)
}

trait JerkUnit[C <: TypeContext] extends UnitOfMeasure[JerkLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new JerkLike[C](t, this)
}

trait JerkOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait JerkUnitT extends JerkUnit[C]

  object Jerk extends Dimension[JerkLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: JerkUnit[C])(
      implicit n: Numeric[A]) =
      new JerkLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Jerk"
    def primaryUnit = MetersPerSecondCubed
    def siUnit = MetersPerSecondCubed
    def units = Set(MetersPerSecondCubed, FeetPerSecondCubed)
  }

  object MetersPerSecondCubed extends JerkUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "m/s³"
  }

  import ops.lengthOps.{ Feet, Meters }

  object FeetPerSecondCubed extends JerkUnitT {
    val symbol = "ft/s³"
    val conversionFactor = Feet.conversionFactor / Meters.conversionFactor
  }

  object JerkConversions {
    lazy val meterPerSecondCubed = MetersPerSecondCubed(1)
    lazy val footPerSecondCubed = FeetPerSecondCubed(1)

    implicit class JerkConversions[A](a: A)(implicit n: Numeric[A]) {
      def metersPerSecondCubed = MetersPerSecondCubed(a)
      def feetPerSecondCubed = FeetPerSecondCubed(a)
    }
  }
}

