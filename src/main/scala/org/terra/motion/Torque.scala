
package org.terra
package motion

import scala.reflect.ClassTag

import org.terra.mass.MomentOfInertiaLike

/**
  *
  * @author paxelord
  * @since 1.3
  *
  * @param value Double
  */
final class TorqueLike[C <: TypeContext](val value: C#T, val unit: TorqueUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[TorqueLike[C], C#T, C] {

  import ops.torqueOps._
  import ops.angularAccelerationOps.RadiansPerSecondSquared

  type MomentOfInertia = MomentOfInertiaLike[C]
  type AngularAcceleration = AngularAccelerationLike[C]

  def dimension: Dimension[TorqueLike[C], C#T, C] = Torque

  def toNewtonMeters = to(NewtonMeters)
  def toPoundFeet = to(PoundFeet)

  def / (that: MomentOfInertia)(implicit ops: TerraOps[C]): AngularAcceleration = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    RadiansPerSecondSquared(
      ops.div[C#T](toNewtonMeters, that.toKilogramsMetersSquared))
  }
}

trait TorqueUnit[C <: TypeContext] extends UnitOfMeasure[TorqueLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new TorqueLike[C](t, this)
}

trait TorqueOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait TorqueUnitT extends TorqueUnit[C]

  object Torque extends Dimension[TorqueLike[C], C#T, C] {
    private[motion] def apply[A](a: A, unit: TorqueUnit[C])(
      implicit n: Numeric[A]) = 
      new TorqueLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Torque"
    def primaryUnit = NewtonMeters
    def siUnit = NewtonMeters
    def units = Set(NewtonMeters, PoundFeet)
  }

  import ops.massOps.Pounds
  import ops.forceOps.{ Newtons, PoundForce }
  import ops.lengthOps.{ Meters, Feet }

  object NewtonMeters extends TorqueUnitT with PrimaryUnit[C#T, C]
      with SiBaseUnit {
    val symbol = Newtons.symbol + "‧" + Meters.symbol
  }

  object PoundFeet extends TorqueUnitT {
    val symbol = Pounds.symbol + "‧" + Feet.symbol
    val conversionFactor = PoundForce.conversionFactor * Feet.conversionFactor
  }

  object TorqueConversions {
    lazy val newtonMeter = NewtonMeters(1)
    lazy val poundFoot = PoundFeet(1)

    implicit class TorqueConversions[A](val a: A)(implicit n: Numeric[A]) {
      def newtonMeters = NewtonMeters(a)
      def poundFeet = PoundFeet(a)
    }
  }
}

