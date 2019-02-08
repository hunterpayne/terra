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

import space.AreaLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class RadianceLike[C <: TypeContext](val value: C#T, val unit: RadianceUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[RadianceLike[C], C#T, C] {

  import ops.radianceOps._
  import ops.radiantIntensityOps.WattsPerSteradian
  import ops.areaOps.SquareMeters

  type Area = AreaLike[C]
  type RadiantIntensity = RadiantIntensityLike[C]

  def dimension: Dimension[RadianceLike[C], C#T, C] = Radiance

  def *(that: Area)(implicit ops: TerraOps[C]): RadiantIntensity = 
    WattsPerSteradian(ops.num.times(
      this.toWattsPerSteradianPerSquareMeter, that.toSquareMeters))
  def /(that: RadiantIntensity)(implicit ops: TerraOps[C]): Area = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    SquareMeters(ops.div[C#T](
      this.toWattsPerSteradianPerSquareMeter, that.toWattsPerSteradian))
  }

  def toWattsPerSteradianPerSquareMeter = to(WattsPerSteradianPerSquareMeter)
}

trait RadianceUnit[C <: TypeContext] 
    extends UnitOfMeasure[RadianceLike[C], C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new RadianceLike[C](t, this)
}

trait RadianceOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait RadianceUnitT extends RadianceUnit[C]

  object Radiance extends Dimension[RadianceLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: RadianceUnit[C])(
      implicit n: Numeric[A], ops: TerraOps[C]) =
      new RadianceLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Radiance"
    def primaryUnit = WattsPerSteradianPerSquareMeter
    def siUnit = WattsPerSteradianPerSquareMeter
    def units = Set(WattsPerSteradianPerSquareMeter)
  }

  import ops.powerOps.Watts
  import ops.areaOps.SquareMeters
  import ops.solidAngleOps.SquareRadians

  object WattsPerSteradianPerSquareMeter extends RadianceUnitT 
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = 
      Watts.symbol + "/" + SquareRadians.symbol + "/" + SquareMeters.symbol
  }

  object RadianceConversions {
    lazy val wattPerSteradianPerSquareMeter = WattsPerSteradianPerSquareMeter(1)

    implicit class RadianceConversions[A](a: A)(implicit n: Numeric[A]) {
      def wattsPerSteradianPerSquareMeter = WattsPerSteradianPerSquareMeter(a)
    }
  }
}


