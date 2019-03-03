
package org.terra
package electro

import space.LengthLike

/**
  *
  * @author Nicolas Vinuesa
  * @since 1.4
  *
  * @param value Double
  */
final class LinearElectricChargeDensityLike[C <: TypeContext](
  val value: C#T, val unit: LinearElectricChargeDensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[LinearElectricChargeDensityLike[C], C#T, C] {

  import ops.linearElectricChargeDensityOps._
  import ops.areaElectricChargeDensityOps.CoulombsPerSquareMeter
  import ops.electricChargeOps.Coulombs

  type Length = LengthLike[C]
  type ElectricCharge = ElectricChargeLike[C]
  type AreaElectricChargeDensity = AreaElectricChargeDensityLike[C]

  def dimension: Dimension[LinearElectricChargeDensityLike[C], C#T, C] = 
    LinearElectricChargeDensity

  def *(that: Length)(implicit ops: TerraOps[C]): ElectricCharge =
    Coulombs(ops.num.times(this.toCoulombsMeters, that.toMeters))
  def /(that: Length)(implicit ops: TerraOps[C]): AreaElectricChargeDensity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CoulombsPerSquareMeter(ops.div[C#T](this.toCoulombsMeters, that.toMeters))
  }

  def toCoulombsMeters = to(CoulombsPerMeter)
}

trait LinearElectricChargeDensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[LinearElectricChargeDensityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new LinearElectricChargeDensityLike[C](t, this)
}

trait LinearElectricChargeDensityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait LinearElectricChargeDensityUnitT 
      extends LinearElectricChargeDensityUnit[C]

  object LinearElectricChargeDensity 
      extends Dimension[LinearElectricChargeDensityLike[C], C#T, C] {
    private[electro] def apply[A](
      a: A, unit: LinearElectricChargeDensityUnit[C])(
      implicit n: Numeric[A]) = 
      new LinearElectricChargeDensityLike[C](
        ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "LinearElectricChargeDensity"
    def primaryUnit = CoulombsPerMeter
    def siUnit = CoulombsPerMeter
    def units = Set(CoulombsPerMeter)
  }

  object CoulombsPerMeter extends LinearElectricChargeDensityUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = ops.electricChargeOps.Coulombs.symbol + "/m"// + ops.lengthOps.Meters.symbol
  }

  object LinearElectricChargeDensityConversions {
    lazy val coulombPerMeter = CoulombsPerMeter(1)

    implicit class LinearElectricChargeDensityConversions[A](a: A)(
      implicit num: Numeric[A]) {
      def coulombsPerMeter = CoulombsPerMeter(a)
    }
  }
}

