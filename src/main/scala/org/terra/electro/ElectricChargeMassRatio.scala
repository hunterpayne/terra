
package org.terra
package electro

import scala.reflect.ClassTag

import mass.MassLike

/**
  *
  * @author Nicolas Vinuesa
  * @since 1.4
  *
  * @param value Double
  */
final class ElectricChargeMassRatioLike[C <: TypeContext](
  val value: C#T, val unit: ElectricChargeMassRatioUnit[C])(
  implicit ops: TerraOps[C])
  extends Quantity[ElectricChargeMassRatioLike[C], C#T, C] {

  import ops.electricChargeMassRatioOps._
  import ops.massOps.Kilograms
  import ops.electricChargeOps.Coulombs

  type Mass = MassLike[C]
  type ElectricCharge = ElectricChargeLike[C]

  def dimension: Dimension[ElectricChargeMassRatioLike[C], C#T, C] = 
    ElectricChargeMassRatio

  def *(that: Mass)(implicit ops: TerraOps[C]): ElectricCharge =
    Coulombs(ops.num.times(this.toCoulombsKilograms, that.toKilograms))

  def toCoulombsKilograms = to(CoulombsPerKilogram)
}

trait ElectricChargeMassRatioUnit[C <: TypeContext] 
    extends UnitOfMeasure[ElectricChargeMassRatioLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new ElectricChargeMassRatioLike[C](t, this)
}

trait ElectricChargeMassRatioOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait ElectricChargeMassRatioUnitT extends ElectricChargeMassRatioUnit[C]

  object ElectricChargeMassRatio 
      extends Dimension[ElectricChargeMassRatioLike[C], C#T, C] {
    private[electro] def apply[A](
      a: A, unit: ElectricChargeMassRatioUnit[C])(
      implicit n: Numeric[A]) = 
      new ElectricChargeMassRatioLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ElectricChargeMassRatio"
    def primaryUnit = CoulombsPerKilogram
    def siUnit = CoulombsPerKilogram
    def units = Set(CoulombsPerKilogram)
  }


  object CoulombsPerKilogram extends ElectricChargeMassRatioUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = ops.electricChargeOps.Coulombs.symbol + "/kg" //+ ops.massOps.Kilograms.symbol
  }

  object ElectricChargeMassRatioConversions {
    lazy val coulombPerKilogram = CoulombsPerKilogram(1)

    implicit class ElectricChargeMassRatioConversions[A](a: A)(implicit num: Numeric[A]) {
      def coulombsPerKilogram = CoulombsPerKilogram(a)
    }
  }
}
