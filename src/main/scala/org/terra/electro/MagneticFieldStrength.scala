
package org.terra
package electro

import scala.reflect.ClassTag

import space.LengthLike

/**
  *
  * @author Nicolas Vinuesa
  * @since 1.4
  *
  * @param value Double
  */
final class MagneticFieldStrengthLike[C <: TypeContext](
  val value: C#T, val unit: MagneticFieldStrengthUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MagneticFieldStrengthLike[C], C#T, C] {

  import ops.magneticFieldStrengthOps._
  import ops.electricCurrentOps.Amperes

  type Length = LengthLike[C]
  type ElectricCurrent = ElectricCurrentLike[C]

  def dimension: Dimension[MagneticFieldStrengthLike[C], C#T, C] = 
    MagneticFieldStrength

  def *(that: Length)(implicit ops: TerraOps[C]): ElectricCurrent =
    Amperes(ops.num.times(this.toAmperesPerMeter, that.toMeters))

  def toAmperesPerMeter = to(AmperesPerMeter)
}

trait MagneticFieldStrengthUnit[C <: TypeContext] 
    extends UnitOfMeasure[MagneticFieldStrengthLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new MagneticFieldStrengthLike[C](t, this)
}

trait MagneticFieldStrengthOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait MagneticFieldStrengthUnitT extends MagneticFieldStrengthUnit[C]

  object MagneticFieldStrength 
      extends Dimension[MagneticFieldStrengthLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: MagneticFieldStrengthUnit[C])(
      implicit n: Numeric[A]) = 
      new MagneticFieldStrengthLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "MagneticFieldStrength"
    def primaryUnit = AmperesPerMeter
    def siUnit = AmperesPerMeter
    def units = Set(AmperesPerMeter)
  }

  object AmperesPerMeter extends MagneticFieldStrengthUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = 
      ops.electricCurrentOps.Amperes.symbol + "/" + ops.lengthOps.Meters.symbol
  }

  object MagneticFieldStrengthConversions {
    lazy val amperePerMeter = AmperesPerMeter(1)

    implicit class MagneticFieldStrengthConversions[A](a: A)(
      implicit num: Numeric[A]) {
      def amperesPerMeter = AmperesPerMeter(a)
    }
  }
}

