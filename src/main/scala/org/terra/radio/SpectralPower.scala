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

import energy.PowerLike
import space.LengthLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class SpectralPowerLike[C <: TypeContext](
  val value: C#T, val unit: SpectralPowerUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[SpectralPowerLike[C], C#T, C] {

  import ops.spectralPowerOps._
  import ops.powerOps.Watts
  import ops.lengthOps.Meters

  type Length = LengthLike[C]
  type Power = PowerLike[C]

  def dimension: Dimension[SpectralPowerLike[C], C#T, C] = SpectralPower

  def *(that: Length)(implicit ops: TerraOps[C]): Power =
    Watts(ops.num.times(this.toWattsPerMeter, that.toMeters))
  def /(that: Power)(implicit ops: TerraOps[C]): Length = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Meters(ops.div[C#T](this.toWattsPerMeter, that.toWatts))
  }

  def toWattsPerMeter = value
}

trait SpectralPowerUnit[C <: TypeContext] 
    extends UnitOfMeasure[SpectralPowerLike[C], C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new SpectralPowerLike[C](t, this)
}

trait SpectralPowerOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait SpectralPowerUnitT extends SpectralPowerUnit[C]

  object SpectralPower extends Dimension[SpectralPowerLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: SpectralPowerUnit[C])(
      implicit n: Numeric[A]) = 
      new SpectralPowerLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "SpectralPower"
    def primaryUnit = WattsPerMeter
    def siUnit = WattsPerMeter
    def units = Set(WattsPerMeter)
  }

  import ops.powerOps.Watts
  import ops.lengthOps.Meters

  object WattsPerMeter extends SpectralPowerUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = Watts.symbol + "/" + Meters.symbol
  }

  object SpectralPowerConversions {
    lazy val wattPerMeter = WattsPerMeter(1)

    implicit class SpectralPowerConversions[A](a: A)(implicit n: Numeric[A]) {
      def wattsPerMeter = WattsPerMeter(a)
    }
  }
}


