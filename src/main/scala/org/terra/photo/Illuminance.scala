/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package photo

import time.TimeDerivative
import space.AreaLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.standard.photo.Lux]]
 */
final class IlluminanceLike[C <: TypeContext](
  val value: C#T, val unit: IlluminanceUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[IlluminanceLike[C], C#T, C]
    with TimeDerivative[LuminousExposureLike[C], C#T, C#T, C] {

  import ops.illuminanceOps._
  import ops.timeOps.Seconds
  import ops.luminousExposureOps.LuxSeconds
  import ops.luminousFluxOps.Lumens

  type Area = AreaLike[C]
  type LuminousFlux = LuminousFluxLike[C]

  def dimension: Dimension[IlluminanceLike[C], C#T, C] = Illuminance

  protected[terra] def timeIntegrated = LuxSeconds(toLux)
  protected[terra] def time = Seconds(1)

  def *(that: Area)(implicit ops: TerraOps[C]): LuminousFlux =
    Lumens(ops.num.times(this.toLux, that.toSquareMeters))

  def toLux = to(Lux)
}

trait IlluminanceUnit[C <: TypeContext] 
    extends UnitOfMeasure[IlluminanceLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new IlluminanceLike[C](t, this)
}

trait IlluminanceOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait IlluminanceUnitT extends IlluminanceUnit[C]

  object Illuminance extends Dimension[IlluminanceLike[C], C#T, C] {
    private[photo] def apply[A](a: A, unit: IlluminanceUnit[C])(
      implicit n: Numeric[A]) = 
      new IlluminanceLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Illuminance"
    def primaryUnit = Lux
    def siUnit = Lux
    def units = Set(Lux)
  }


  object Lux extends IlluminanceUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "lx"
  }

  object IlluminanceConversions {
    lazy val lux = Lux(1)

    implicit class IlluminanceConversions[A](a: A)(implicit n: Numeric[A]) {
      def lux = Lux(a)
    }
  }
}

