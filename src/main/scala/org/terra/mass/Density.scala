/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package mass

import space.{ VolumeLike, SpecificVolumeLike }

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class DensityLike[C <: TypeContext](
  val value: C#T, val unit: DensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[DensityLike[C], C#T, C] {

  import ops.densityOps._
  import ops.massOps.Kilograms
  import ops.specificVolumeOps.CubicMetersPerKilogram

  type Volume = VolumeLike[C]
  type Mass = MassLike[C]
  type SpecificVolume = SpecificVolumeLike[C]

  def dimension: Dimension[DensityLike[C], C#T, C] = Density

  def *(that: Volume)(implicit ops: TerraOps[C]): Mass = 
    Kilograms(ops.num.times(this.value, that.toCubicMeters))

  def inv(implicit ops: TerraOps[C]): SpecificVolume = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CubicMetersPerKilogram(ops.div[C#T](
      ops.convDouble(1.0), toKilogramsPerCubicMeter))
  }

  def toKilogramsPerCubicMeter = to(KilogramsPerCubicMeter)
}

trait DensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[DensityLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new DensityLike[C](t, this)
}

trait DensityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait DensityUnitT extends DensityUnit[C]

  object Density extends Dimension[DensityLike[C], C#T, C] {
    private[mass] def apply[A](a: A, unit: DensityUnit[C])(
      implicit n: Numeric[A]) =
      new DensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(m: MassLike[C], v: VolumeLike[C])(
      implicit ops: TerraOps[C]): DensityLike[C] = {
      implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: PseudoClassTag[C#T] = ops.getClassTagT
      KilogramsPerCubicMeter(ops.div[C#T](m.toKilograms, v.toCubicMeters))
    }
    def apply(value: Any) = parse(value)
    def name = "Density"
    def primaryUnit = KilogramsPerCubicMeter
    def siUnit = KilogramsPerCubicMeter
    def units = Set(KilogramsPerCubicMeter)
  }

  object KilogramsPerCubicMeter extends DensityUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "kg/m³"
  }

  object DensityConversions {
    lazy val kilogramPerCubicMeter = KilogramsPerCubicMeter(1)

    implicit class DensityConversions[A](a: A)(implicit n: Numeric[A]) {
      def kilogramsPerCubicMeter = KilogramsPerCubicMeter(a)
    }
  }
}

