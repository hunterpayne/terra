
package org.terra
package electro

import space.{ VolumeLike, LengthLike, AreaLike }

/**
  *
  * @author Nicolas Vinuesa
  * @since 1.4
  *
  * @param value Double
  */
final class ElectricChargeDensityLike[C <: TypeContext](
  val value: C#T, val unit: ElectricChargeDensityUnit[C])(
  implicit ops: TerraOps[C])
  extends Quantity[ElectricChargeDensityLike[C], C#T, C] {

  import ops.electricChargeDensityOps._
  import ops.electricChargeOps.Coulombs
  import ops.linearElectricChargeDensityOps.CoulombsPerMeter
  import ops.areaElectricChargeDensityOps.CoulombsPerSquareMeter

  type Length = LengthLike[C]
  type Area = AreaLike[C]
  type Volume = VolumeLike[C]
  type ElectricCharge = ElectricChargeLike[C]
  type LinearElectricChargeDensity = LinearElectricChargeDensityLike[C]
  type AreaElectricChargeDensity = AreaElectricChargeDensityLike[C]

  def dimension: Dimension[ElectricChargeDensityLike[C], C#T, C] = 
    ElectricChargeDensity

  def *(that: Volume)(implicit ops: TerraOps[C]): ElectricCharge = 
    Coulombs(ops.num.times(this.toCoulombsCubicMeters, that.toCubicMeters))
  def *(that: Area)(implicit ops: TerraOps[C]): LinearElectricChargeDensity = 
    CoulombsPerMeter(ops.num.times(this.toCoulombsCubicMeters, that.toSquareMeters))
  def *(that: Length)(implicit ops: TerraOps[C]): AreaElectricChargeDensity =
    CoulombsPerSquareMeter(ops.num.times(this.toCoulombsCubicMeters, that.toMeters))

  def toCoulombsCubicMeters = to(CoulombsPerCubicMeter)
}

trait ElectricChargeDensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[ElectricChargeDensityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) =
    new ElectricChargeDensityLike[C](t, this)
}

trait ElectricChargeDensityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait ElectricChargeDensityUnitT extends ElectricChargeDensityUnit[C]

  object ElectricChargeDensity 
      extends Dimension[ElectricChargeDensityLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: ElectricChargeDensityUnit[C])(
      implicit n: Numeric[A]) = 
      new ElectricChargeDensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ElectricChargeDensity"
    def primaryUnit = CoulombsPerCubicMeter
    def siUnit = CoulombsPerCubicMeter
    def units = Set(CoulombsPerCubicMeter)
  }

  object CoulombsPerCubicMeter extends ElectricChargeDensityUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = ops.electricChargeOps.Coulombs.symbol + "/m³" //+ ops.lengthOps.Meters.symbol + "³"
  }

  object ElectricChargeDensityConversions {
    lazy val coulombPerCubicMeter = CoulombsPerCubicMeter(1)

    implicit class ElectricChargeDensityConversions[A](a: A)(
      implicit num: Numeric[A]) {
      def coulombsPerCubicMeter = CoulombsPerCubicMeter(a)
    }
  }
}

