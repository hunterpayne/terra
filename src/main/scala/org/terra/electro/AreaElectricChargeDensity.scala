
package org.terra
package electro

import scala.reflect.ClassTag

import space.{ AreaLike, LengthLike }

/**
  *
  * @author Nicolas Vinuesa
  * @since 1.4
  *
  * @param value Double
  */
final class AreaElectricChargeDensityLike[C <: TypeContext](
  val value: C#T, val unit: AreaElectricChargeDensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[AreaElectricChargeDensityLike[C], C#T, C] {

  type T = C#T

  import ops.areaElectricChargeDensityOps._
  import ops.linearElectricChargeDensityOps.CoulombsPerMeter
  import ops.electricChargeOps.Coulombs

  type Area = AreaLike[C]
  type Length = LengthLike[C]
  type ElectricCharge = ElectricChargeLike[C]
  type LinearElectricChargeDensity = LinearElectricChargeDensityLike[C]

  def dimension: Dimension[AreaElectricChargeDensityLike[C], T, C] = 
    AreaElectricChargeDensity

  def *(that: Area)(implicit ops: TerraOps[C]): ElectricCharge = 
    Coulombs(ops.num.times(this.toCoulombsSquareMeters, that.toSquareMeters))
  def *(that: Length)(
    implicit ops: TerraOps[C]): LinearElectricChargeDensity =
    CoulombsPerMeter(ops.num.times(this.toCoulombsSquareMeters, that.toMeters))

  def toCoulombsSquareMeters = to(CoulombsPerSquareMeter)
}

trait AreaElectricChargeDensityUnit[C <: TypeContext]
    extends UnitOfMeasure[AreaElectricChargeDensityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new AreaElectricChargeDensityLike[C](t, this)
}

trait AreaElectricChargeDensityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait AreaElectricChargeDensityUnitT extends AreaElectricChargeDensityUnit[C]

  object AreaElectricChargeDensity 
      extends Dimension[AreaElectricChargeDensityLike[C], C#T, C] {
    private[electro] def apply[A](
      a: A, unit: AreaElectricChargeDensityUnit[C])(
      implicit n: Numeric[A]) =
      new AreaElectricChargeDensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "AreaElectricChargeDensity"
    def primaryUnit = CoulombsPerSquareMeter
    def siUnit = CoulombsPerSquareMeter
    def units = Set(CoulombsPerSquareMeter)
  }

  object CoulombsPerSquareMeter 
      extends AreaElectricChargeDensityUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = 
      ops.electricChargeOps.Coulombs.symbol + "/" + 
        ops.lengthOps.Meters.symbol + "²"
  }

  object AreaElectricChargeDensityConversions {
    lazy val coulombPerSquareMeter = CoulombsPerSquareMeter(1)

    implicit class AreaElectricChargeDensityConversions[A](a: A)(
      implicit num: Numeric[A]) {
      def coulombsPerSquareMeter = CoulombsPerSquareMeter(a)
    }
  }
}

