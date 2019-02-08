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

import time.{ TimeDerivative, TimeIntegral, TimeLike }
import space.{ AngleLike, LengthLike }

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 *
 */
final class AngularVelocityLike[C <: TypeContext](
  val value: C#T, val unit: AngularVelocityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[AngularVelocityLike[C], C#T, C] 
    with TimeDerivative[AngleLike[C], C#T, C#T, C]
    with TimeIntegral[AngularAccelerationLike[C], C#T, C#T, C] {

  import ops.angularVelocityOps._
  import ops.timeOps.Seconds
  import ops.angularAccelerationOps.RadiansPerSecondSquared
  import ops.angleOps.Radians

  type Length = LengthLike[C]
  type Angle = AngleLike[C]
  type Time = TimeLike[C]
  type Velocity = VelocityLike[C]
  type AngularAcceleration = AngularAccelerationLike[C]

  def dimension: Dimension[AngularVelocityLike[C], C#T, C] = AngularVelocity

  def toRadiansPerSecond = to(RadiansPerSecond)
  def toDegreesPerSecond = to(DegreesPerSecond)
  def toGradiansPerSecond = to(GradiansPerSecond)
  def toTurnsPerSecond = to(TurnsPerSecond)

  /**
    * linear velocity of an object rotating with this angular velocity
    * and the given radius from the center of rotation
    * @param radius the distance from the center of rotation
    * @return linear velocity with given angular velocity and radius
    */
  def onRadius(radius: Length)(implicit ops: TerraOps[C]): Velocity =
    (radius / Seconds(1)) * toRadiansPerSecond

  protected[terra] def timeIntegrated: Angle = {
    implicit val opsArg = ops
    Radians(toRadiansPerSecond)
  }

  protected[terra] def timeDerived: AngularAcceleration = {
    implicit val opsArg = ops
    RadiansPerSecondSquared(toRadiansPerSecond)
  }

  protected[terra] def time: Time = {
    implicit val opsArg = ops
    Seconds(1)
  }
}

trait AngularVelocityUnit[C <: TypeContext] 
    extends UnitOfMeasure[AngularVelocityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new AngularVelocityLike[C](t, this)
}

trait AngularVelocityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait AngularVelocityUnitT extends AngularVelocityUnit[C]

  object AngularVelocity extends Dimension[AngularVelocityLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: AngularVelocityUnit[C])(
      implicit n: Numeric[A]) = 
      new AngularVelocityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "AngularVelocity"
    def primaryUnit = RadiansPerSecond
    def siUnit = RadiansPerSecond
    def units = Set(
      RadiansPerSecond, DegreesPerSecond, GradiansPerSecond, TurnsPerSecond)
  }

  object RadiansPerSecond extends AngularVelocityUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "rad/s"
  }

  import ops.angleOps.{ Degrees, Radians, Gradians, Turns }

  object DegreesPerSecond extends AngularVelocityUnitT {
    val symbol = "°/s"
    val conversionFactor = Degrees.conversionFactor / Radians.conversionFactor
  }

  object GradiansPerSecond extends AngularVelocityUnitT {
    val symbol = "grad/s"
    val conversionFactor = Gradians.conversionFactor / Radians.conversionFactor
  }

  object TurnsPerSecond extends AngularVelocityUnitT {
    val symbol = "turns/s"
    val conversionFactor = Turns.conversionFactor / Radians.conversionFactor
  }

  object AngularVelocityConversions {
    lazy val radianPerSecond = RadiansPerSecond(1)
    lazy val degreePerSecond = DegreesPerSecond(1)
    lazy val gradPerSecond = GradiansPerSecond(1)
    lazy val gradianPerSecond = GradiansPerSecond(1)
    lazy val turnPerSecond = TurnsPerSecond(1)

    implicit class AngularVelocityConversions[A](a: A)(implicit n: Numeric[A]) {
      def radiansPerSecond = RadiansPerSecond(a)
      def degreesPerSecond = DegreesPerSecond(a)
      def gradiansPerSecond = GradiansPerSecond(a)
      def turnsPerSecond = TurnsPerSecond(a)
    }
  }
}

