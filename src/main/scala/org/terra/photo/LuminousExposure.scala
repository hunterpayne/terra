/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package photo

import time.TimeIntegral

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.standard.photo.LuxSeconds]]
 */
final class LuminousExposureLike[C <: TypeContext](
  val value: C#T, val unit: LuminousExposureUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[LuminousExposureLike[C], C#T, C]
    with TimeIntegral[IlluminanceLike[C], C#T, C#T, C] {

  import ops.luminousExposureOps._
  import ops.timeOps.Seconds
  import ops.illuminanceOps.Lux

  def dimension: Dimension[LuminousExposureLike[C], C#T, C] = LuminousExposure

  protected def timeDerived = Lux(toLuxSeconds)
  protected[terra] def time = Seconds(1)

  def toLuxSeconds = to(LuxSeconds)
}

trait LuminousExposureUnit[C <: TypeContext] 
    extends UnitOfMeasure[LuminousExposureLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new LuminousExposureLike[C](t, this)
}

trait LuminousExposureOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait LuminousExposureUnitT extends LuminousExposureUnit[C]

  object LuminousExposure extends Dimension[LuminousExposureLike[C], C#T, C] {
    private[photo] def apply[A](a: A, unit: LuminousExposureUnit[C])(
      implicit n: Numeric[A]) =
      new LuminousExposureLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "LuminousExposure"
    def primaryUnit = LuxSeconds
    def siUnit = LuxSeconds
    def units = Set(LuxSeconds)
  }


  object LuxSeconds extends LuminousExposureUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "lx⋅s"
  }

  object LuminousExposureConversions {
    lazy val luxSecond = LuxSeconds(1)

    implicit class LuminousExposureConversions[A](a: A)(implicit n: Numeric[A]) {
      def luxSeconds = LuxSeconds(a)
    }
  }
}
