
package org.terra
package motion

import scala.reflect.ClassTag

import mass.MomentOfInertiaLike
import space.LengthLike
import org.terra.time.{ TimeLike, TimeDerivative }

/**
  *
  * @author paxelord
  * @since 1.3
  *
  * @param value Double
  */
final class AngularAccelerationLike[C <: TypeContext](
  val value: C#T, val unit: AngularAccelerationUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[AngularAccelerationLike[C], C#T, C] 
    with TimeDerivative[AngularVelocityLike[C], C#T, C#T, C] {

  import ops.angularAccelerationOps._
  import ops.timeOps.Seconds
  import ops.angularVelocityOps.RadiansPerSecond
  import ops.torqueOps.NewtonMeters

  type Torque = TorqueLike[C]
  type MomentOfInertia = MomentOfInertiaLike[C]
  type AngularVelocity = AngularVelocityLike[C]
  type Length = LengthLike[C]
  type Acceleration = AccelerationLike[C]

  def dimension: Dimension[AngularAccelerationLike[C], C#T, C] =
    AngularAcceleration

  def toRadiansPerSecondSquared = to(RadiansPerSecondSquared)
  def toDegreesPerSecondSquared = to(DegreesPerSecondSquared)
  def toGradsPerSecondSquared = to(GradiansPerSecondSquared)
  def toTurnsPerSecondSquared = to(TurnsPerSecondSquared)
  def toArcminutesPerSecondSquared = to(ArcminutesPerSecondSquared)
  def toArcsecondsPerSecondSquared = to(ArcsecondsPerSecondSquared)

  /**
    * linear acceleration of an object rotating with this angular acceleration
    * and the given radius from the center of rotation
    * @param radius the distance from the center of rotation
    * @return linear acceleration with given angular acceleration and radius
    */
  def onRadius(radius: Length)(implicit ops: TerraOps[C]): Acceleration =
    (radius / Seconds(1).squared) * toRadiansPerSecondSquared

  def *(that: MomentOfInertia)(implicit ops: TerraOps[C]): Torque =
    NewtonMeters(ops.num.times(
      this.toRadiansPerSecondSquared, that.toKilogramsMetersSquared))

  override protected[terra] def timeIntegrated: AngularVelocity = {
    implicit val opsArg = ops
    RadiansPerSecond(toRadiansPerSecondSquared)
  }

  override protected[terra] def time: Time = {
    implicit val opsArg = ops
    Seconds(1)
  }
}

trait AngularAccelerationUnit[C <: TypeContext]
    extends UnitOfMeasure[AngularAccelerationLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new AngularAccelerationLike[C](t, this)
}

trait AngularAccelerationOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait AngularAccelerationUnitT extends AngularAccelerationUnit[C]

  object AngularAcceleration 
      extends Dimension[AngularAccelerationLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: AngularAccelerationUnit[C])(
      implicit n: Numeric[A], ops: TerraOps[C]) =
      new AngularAccelerationLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "AngularAcceleration"
    def primaryUnit = RadiansPerSecondSquared
    def siUnit = RadiansPerSecondSquared
    def units = Set(
      RadiansPerSecondSquared,
      DegreesPerSecondSquared,
      GradiansPerSecondSquared,
      TurnsPerSecondSquared,
      ArcminutesPerSecondSquared,
      ArcsecondsPerSecondSquared)
  }

  import ops.angleOps.{ Radians, Degrees, Gradians, Turns, Arcminutes, Arcseconds }

  object RadiansPerSecondSquared extends AngularAccelerationUnitT 
      with PrimaryUnit[C#T, C] with SiUnit{
    val symbol = Radians.symbol + "/s²"
  }

  object DegreesPerSecondSquared extends AngularAccelerationUnitT {
    val symbol = Degrees.symbol + "/s²"
    val conversionFactor = Degrees.conversionFactor
  }

  object GradiansPerSecondSquared extends AngularAccelerationUnitT {
    val symbol = Gradians.symbol + "/s²"
    val conversionFactor = Gradians.conversionFactor
  }

  object TurnsPerSecondSquared extends AngularAccelerationUnitT {
    val symbol = Turns.symbol + "/s²"
    val conversionFactor = Turns.conversionFactor
  }

  object ArcminutesPerSecondSquared extends AngularAccelerationUnitT {
    val symbol = Arcminutes.symbol + "/s²"
    val conversionFactor = Arcminutes.conversionFactor
  }

  object ArcsecondsPerSecondSquared extends AngularAccelerationUnitT {
    val symbol = Arcseconds.symbol + "/s²"
    val conversionFactor = Arcseconds.conversionFactor
  }

  object AngularAccelerationConversions {
    lazy val radianPerSecondSquared = RadiansPerSecondSquared(1)
    lazy val degreePerSecondSquared = DegreesPerSecondSquared(1)
    lazy val gradPerSecondSquared = GradiansPerSecondSquared(1)
    lazy val turnPerSecondSquared = TurnsPerSecondSquared(1)

    implicit class AngularAccelerationConversions[A](val a: A)(
      implicit n: Numeric[A]) {
      def radiansPerSecondSquared = RadiansPerSecondSquared(a)
      def degreesPerSecondSquared = DegreesPerSecondSquared(a)
      def gradsPerSecondSquared = GradiansPerSecondSquared(a)
      def turnsPerSecondSquared = TurnsPerSecondSquared(a)
    }
  }
}

