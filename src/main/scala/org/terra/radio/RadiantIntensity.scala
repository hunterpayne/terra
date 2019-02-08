/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package radio

import scala.reflect.ClassTag

import org.terra.energy.PowerLike
import space.{ SolidAngleLike, LengthLike, AreaLike }

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class RadiantIntensityLike[C <: TypeContext](
  val value: C#T, val unit: RadiantIntensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[RadiantIntensityLike[C], C#T, C] {

  import ops.radiantIntensityOps._
  import ops.powerOps.Watts
  import ops.solidAngleOps.SquareRadians
  import ops.spectralIntensityOps.WattsPerSteradianPerMeter
  import ops.lengthOps.Meters
  import ops.radianceOps.WattsPerSteradianPerSquareMeter
  import ops.areaOps.SquareMeters

  type SolidAngle = SolidAngleLike[C]
  type Power = PowerLike[C]
  type Length = LengthLike[C]
  type SpectralIntensity = SpectralIntensityLike[C]
  type Area = AreaLike[C]
  type Radiance = RadianceLike[C]

  def dimension: Dimension[RadiantIntensityLike[C], C#T, C] = RadiantIntensity

  def *(that: SolidAngle)(implicit ops: TerraOps[C]): Power = 
    Watts(ops.num.times(this.toWattsPerSteradian, that.toSquareRadians))
  def /(that: Power)(implicit ops: TerraOps[C]): SolidAngle = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    SquareRadians(ops.div[C#T](this.toWattsPerSteradian, that.toWatts))
  }
  def /(that: Length)(implicit ops: TerraOps[C]): SpectralIntensity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    WattsPerSteradianPerMeter(ops.div[C#T](this.toWattsPerSteradian, that.toMeters))
  }
  def /(that: SpectralIntensity)(implicit ops: TerraOps[C]): Length = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Meters(ops.div[C#T](this.toWattsPerSteradian, that.toWattsPerSteradianPerMeter))
  }
  def /(that: Area)(implicit ops: TerraOps[C]): Radiance = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    WattsPerSteradianPerSquareMeter(ops.div[C#T](this.toWattsPerSteradian, that.toSquareMeters))
  }
  def /(that: Radiance)(implicit ops: TerraOps[C]): Area = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    SquareMeters(ops.div[C#T](this.toWattsPerSteradian, that.toWattsPerSteradianPerSquareMeter))
  }

  def toWattsPerSteradian = to(WattsPerSteradian)
}

trait RadiantIntensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[RadiantIntensityLike[C], C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new RadiantIntensityLike[C](t, this)
}

trait RadiantIntensityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait RadiantIntensityUnitT extends RadiantIntensityUnit[C]

  object RadiantIntensity extends Dimension[RadiantIntensityLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: RadiantIntensityUnit[C])(
      implicit n: Numeric[A]) = 
      new RadiantIntensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "RadiantIntensity"
    def primaryUnit = WattsPerSteradian
    def siUnit = WattsPerSteradian
    def units = Set(WattsPerSteradian)
  }

  import ops.powerOps.Watts
  import ops.solidAngleOps.SquareRadians

  object WattsPerSteradian extends RadiantIntensityUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = Watts.symbol + "/" + SquareRadians.symbol
  }

  object RadiantIntensityConversions {
    lazy val wattPerSteradian = WattsPerSteradian(1)

    implicit class RadiantIntensityConversions[A](a: A)(implicit n: Numeric[A]) {
      def wattsPerSteradian = WattsPerSteradian(a)
    }
  }
}


