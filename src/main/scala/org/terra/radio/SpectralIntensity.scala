/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package radio

import space.LengthLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class SpectralIntensityLike[C <: TypeContext](
  val value: C#T, val unit: SpectralIntensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[SpectralIntensityLike[C], C#T, C] {

  import ops.spectralIntensityOps._
  import ops.radiantIntensityOps.WattsPerSteradian
  import ops.lengthOps.Meters

  type Length = LengthLike[C]
  type RadiantIntensity = RadiantIntensityLike[C]

  def dimension: Dimension[SpectralIntensityLike[C], C#T, C] = SpectralIntensity

  def *(that: Length)(implicit ops: TerraOps[C]): RadiantIntensity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    WattsPerSteradian(ops.num.times(this.toWattsPerSteradianPerMeter, that.toMeters))
  }
  def /(that: RadiantIntensity)(implicit ops: TerraOps[C]): Length = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Meters(ops.div[C#T](this.toWattsPerSteradianPerMeter, that.toWattsPerSteradian))
  }

  def toWattsPerSteradianPerMeter = to(WattsPerSteradianPerMeter)
}

trait SpectralIntensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[SpectralIntensityLike[C], C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new SpectralIntensityLike[C](t, this)
}

trait SpectralIntensityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait SpectralIntensityUnitT extends SpectralIntensityUnit[C]

  object SpectralIntensity extends Dimension[SpectralIntensityLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: SpectralIntensityUnit[C])(
      implicit n: Numeric[A], ops: TerraOps[C]) =
      new SpectralIntensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "SpectralIntensity"
    def primaryUnit = WattsPerSteradianPerMeter
    def siUnit = WattsPerSteradianPerMeter
    def units = Set(WattsPerSteradianPerMeter)
  }

  import ops.powerOps.Watts
  import ops.solidAngleOps.SquareRadians
  import ops.lengthOps.Meters

  object WattsPerSteradianPerMeter extends SpectralIntensityUnitT
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = 
      Watts.symbol + "/" + SquareRadians.symbol + "/" + Meters.symbol
  }

  object SpectralIntensityConversions {
    lazy val wattPerSteradianPerMeter = WattsPerSteradianPerMeter(1)

    implicit class SpectralIntensityConversions[A](a: A)(implicit n: Numeric[A]) {
      def wattsPerSteradianPerMeter = WattsPerSteradianPerMeter(a)
    }
  }
}


