/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package space

import energy.{ PowerLike }
import photo.{ LuminousFluxLike, LuminousIntensityLike }
import radio.RadiantIntensityLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.space.SquareRadians]]
 */
final class SolidAngleLike[C <: TypeContext](
  val value: C#T, val unit: SolidAngleUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[SolidAngleLike[C], C#T, C] {

  import ops.solidAngleOps._
  import ops.luminousFluxOps.Lumens
  import ops.powerOps.Watts

  type LuminousIntensity = LuminousIntensityLike[C]
  type LuminousFlux = LuminousFluxLike[C]
  type RadiantIntensity = RadiantIntensityLike[C]
  type Power = PowerLike[C]

  def dimension: Dimension[SolidAngleLike[C], C#T, C] = SolidAngle

  def *(that: LuminousIntensity)(implicit ops: TerraOps[C]): LuminousFlux = 
    Lumens(ops.num.times(this.toSquareRadians, that.toCandelas))
  def *(that: RadiantIntensity)(implicit ops: TerraOps[C]): Power = 
    Watts(ops.num.times(this.toSquareRadians, that.toWattsPerSteradian))

  def toSquareRadians = value
  def toSteradians = value
}

trait SolidAngleUnit[C <: TypeContext] 
    extends UnitOfMeasure[SolidAngleLike[C], C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new SolidAngleLike[C](t, this)
}

trait SolidAngleOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait SolidAngleUnitT extends SolidAngleUnit[C]

  object SolidAngle extends Dimension[SolidAngleLike[C], C#T, C] {
    private[space] def apply[A](a: A, unit: SolidAngleUnit[C])(
      implicit n: Numeric[A]) = 
      new SolidAngleLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "SolidAngle"
    def primaryUnit = SquareRadians
    def siUnit = SquareRadians
    def units = Set(SquareRadians)
  }

  object SquareRadians extends SolidAngleUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "sr"
  }

  object SolidAngleConversions {
    lazy val squareRadian = SquareRadians(1)
    lazy val steradian = SquareRadians(1)

    implicit class SolidAngleConversions[A](a: A)(implicit n: Numeric[A]) {
      def squareRadians = SquareRadians(a)
      def steradians = SquareRadians(a)
    }
  }
}


