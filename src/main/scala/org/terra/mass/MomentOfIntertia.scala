
package org.terra
package mass

import motion.{ AngularAccelerationLike, TorqueLike }
import space.LengthLike

/**
  *
  * @author paxelord
  * @since 1.3
  *
  * @param value Double
  */
final class MomentOfInertiaLike[C <: TypeContext](
  val value: C#T, val unit: MomentOfInertiaUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MomentOfInertiaLike[C], C#T, C] {

  import ops.momentOfInertiaOps._
  import ops.massOps.Kilograms
  import ops.torqueOps.NewtonMeters

  type AngularAcceleration = AngularAccelerationLike[C]
  type Torque = TorqueLike[C]
  type Length = LengthLike[C]
  type Mass = MassLike[C]

  def dimension: Dimension[MomentOfInertiaLike[C], C#T, C] = MomentOfInertia

  def toKilogramsMetersSquared = to(KilogramsMetersSquared)
  def toPoundsSquareFeet = to(PoundsSquareFeet)

  def *(angularAcceleration: AngularAcceleration)(
    implicit ops: TerraOps[C]): Torque = {
    val radiansPerSecondSquared = angularAcceleration.toRadiansPerSecondSquared
    NewtonMeters(ops.num.times(toKilogramsMetersSquared, radiansPerSecondSquared))
  }

  /**
    * For a point mass with the given MomentOfInertia rotating with a center of
    * rotation at the given radius, return the mass of the point mass
    * @param radius distance to axis of rotation
    * @return mass of point mass with given radius and MomentOfInertia
    */
  def atCenter(radius: Length)(implicit ops: TerraOps[C]): Mass = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Kilograms(
      ops.div[C#T](toKilogramsMetersSquared, radius.squared.toSquareMeters))
  }
}

trait MomentOfInertiaUnit[C <: TypeContext] 
    extends UnitOfMeasure[MomentOfInertiaLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) =
    new MomentOfInertiaLike[C](t, this)
}

trait MomentOfInertiaOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait MomentOfInertiaUnitT extends MomentOfInertiaUnit[C]

  object MomentOfInertia extends Dimension[MomentOfInertiaLike[C], C#T, C] {
    private[mass] def apply[A](a: A, unit: MomentOfInertiaUnit[C])(
      implicit n: Numeric[A], ops: TerraOps[C]) = 
      new MomentOfInertiaLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "MomentOfInertia"
    def primaryUnit = KilogramsMetersSquared
    def siUnit = KilogramsMetersSquared
    def units = Set(KilogramsMetersSquared, PoundsSquareFeet)
  }

  import ops.massOps.{ Kilograms, Pounds }
  import ops.lengthOps.{ Meters, Feet }

  object KilogramsMetersSquared extends MomentOfInertiaUnitT 
      with PrimaryUnit[C#T, C] with SiBaseUnit {
    val symbol = Kilograms.symbol + "‧" + Meters.symbol + "²"
  }

  object PoundsSquareFeet extends MomentOfInertiaUnitT {
    val symbol = Pounds.symbol + "‧" + Feet.symbol + "²"
    val conversionFactor =
      Pounds.conversionFactor * math.pow(Feet.conversionFactor, 2D)
  }

  object MomentOfInertiaConversions {
    lazy val kilogramMetersSquared = KilogramsMetersSquared(1)
    lazy val poundSquareFeet = PoundsSquareFeet(1)

    implicit class MomentOfInertiaConversions[A](a: A)(
      implicit n: Numeric[A]) {
      def kilogramMetersSquared = KilogramsMetersSquared(a)
      def poundSquareFeet = PoundsSquareFeet(a)
    }
  }
}

