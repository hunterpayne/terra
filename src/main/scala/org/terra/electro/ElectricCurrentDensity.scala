
package org.terra
package electro

import space.{ AreaLike, LengthLike }

/**
  *
  * @author Nicolas Vinuesa
  * @since 1.4
  *
  * @param value Double
  */
final class ElectricCurrentDensityLike[C <: TypeContext](
  val value: C#T, val unit: ElectricCurrentDensityUnit[C])(
  implicit ops: TerraOps[C])
  extends Quantity[ElectricCurrentDensityLike[C], C#T, C] {

  import ops.electricCurrentDensityOps._
  import ops.electricCurrentOps.Amperes
  import ops.magneticFieldStrengthOps.AmperesPerMeter

  type Area = AreaLike[C]
  type ElectricCurrent = ElectricCurrentLike[C]
  type Length = LengthLike[C]
  type MagneticFieldStrength = MagneticFieldStrengthLike[C]

  def dimension: Dimension[ElectricCurrentDensityLike[C], C#T, C] = 
    ElectricCurrentDensity

  def *(that: Area)(implicit ops: TerraOps[C]): ElectricCurrent = 
    Amperes(ops.num.times(this.toAmperesPerSquareMeter, that.toSquareMeters))
  def *(that: Length)(implicit ops: TerraOps[C]): MagneticFieldStrength = 
    AmperesPerMeter(ops.num.times(this.toAmperesPerSquareMeter, that.toMeters))

  def toAmperesPerSquareMeter = to(AmperesPerSquareMeter)
}

trait ElectricCurrentDensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[ElectricCurrentDensityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new ElectricCurrentDensityLike[C](t, this)
}

trait ElectricCurrentDensityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait ElectricCurrentDensityUnitT extends ElectricCurrentDensityUnit[C]

  object ElectricCurrentDensity 
      extends Dimension[ElectricCurrentDensityLike[C], C#T, C] {
    private[electro] def apply[A](
      a: A, unit: ElectricCurrentDensityUnit[C])(
      implicit n: Numeric[A]) = 
      new ElectricCurrentDensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ElectricCurrentDensity"
    def primaryUnit = AmperesPerSquareMeter
    def siUnit = AmperesPerSquareMeter
    def units = Set(AmperesPerSquareMeter)
  }


  object AmperesPerSquareMeter extends ElectricCurrentDensityUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = ops.electricCurrentOps.Amperes.symbol + "/" + 
      ops.lengthOps.Meters.symbol + "²"
  }

  object ElectricCurrentDensityConversions {
    lazy val amperePerSquareMeter = AmperesPerSquareMeter(1)

    implicit class ElectricCurrentDensityConversions[A](a: A)(
      implicit num: Numeric[A]) {
      def amperesPerSquareMeter = AmperesPerSquareMeter(a)
    }
  }
}
