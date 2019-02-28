
package org.terra
package space

import scala.reflect.ClassTag

import mass.ChemicalAmountLike

/**
 */
final class MolarVolumeLike[C <: TypeContext](
  val value: C#T, val unit: MolarVolumeUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MolarVolumeLike[C], C#T, C] {

  import ops.molarVolumeOps._
  import ops.volumeOps.CubicMeters
  import ops.chemicalAmountOps.Moles
  import ops.volumeOps.CubicMeters

  type Volume = VolumeLike[C]
  type ChemicalAmount = ChemicalAmountLike[C]

  def dimension: Dimension[MolarVolumeLike[C], C#T, C] = MolarVolume

  def *(that: ChemicalAmount)(implicit ops: TerraOps[C]): Volume =
    CubicMeters(ops.num.times(this.toCubicMetersPerMole, that.toMoles))

  def toCubicMetersPerMole = to(CubicMetersPerMole)
}

trait MolarVolumeUnit[C <: TypeContext] 
    extends UnitOfMeasure[MolarVolumeLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new MolarVolumeLike[C](t, this)
}

trait MolarVolumeOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait MolarVolumeUnitT extends MolarVolumeUnit[C]

  object MolarVolume extends Dimension[MolarVolumeLike[C], C#T, C] 
      with BaseDimension {
    private[space] def apply[A](a: A, unit: MolarVolumeUnit[C])(
      implicit n: Numeric[A]) = 
      new MolarVolumeLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    val name = "MolarVolume"
    def primaryUnit = CubicMetersPerMole
    def siUnit = CubicMetersPerMole
    def units = Set(CubicMetersPerMole)
    def dimensionSymbol = "M"
  }

  import ops.volumeOps.CubicMeters
  import ops.chemicalAmountOps.Moles

  object CubicMetersPerMole extends MolarVolumeUnitT with PrimaryUnit[C#T, C]
      with SiBaseUnit {
    val symbol = CubicMeters.symbol + "/" + Moles.symbol
  }

  object MolarVolumeConversions {
    lazy val cubicMeterPerMole = CubicMetersPerMole(1)

    implicit class MolarVolumeConversions[A](a: A)(implicit n: Numeric[A]) {
      def cubicMetersPerMole = CubicMetersPerMole(a)
    }
  }
}
