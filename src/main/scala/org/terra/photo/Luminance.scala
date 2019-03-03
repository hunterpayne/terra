/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package photo

import space.AreaLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class LuminanceLike[C <: TypeContext](
  val value: C#T, val unit: LuminanceUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[LuminanceLike[C], C#T, C] {

  import ops.luminanceOps._
  import ops.luminousIntensityOps.Candelas

  type Area = AreaLike[C]
  type LuminousIntensity = LuminousIntensityLike[C]

  def dimension: Dimension[LuminanceLike[C], C#T, C] = Luminance

  def *(that: Area)(implicit ops: TerraOps[C]): LuminousIntensity = 
    Candelas(ops.num.times(this.value, that.toSquareMeters))

  def toCandelasPerSquareMeters = to(CandelasPerSquareMeter)
}

trait LuminanceUnit[C <: TypeContext] 
    extends UnitOfMeasure[LuminanceLike[C], C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new LuminanceLike[C](t, this)
}

trait LuminanceOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait LuminanceUnitT extends LuminanceUnit[C]

  object Luminance extends Dimension[LuminanceLike[C], C#T, C] {
    private[photo] def apply[A](a: A, unit: LuminanceUnit[C])(
      implicit n: Numeric[A]) =
      new LuminanceLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)

    def name = "Luminance"
    def primaryUnit = CandelasPerSquareMeter
    def siUnit = CandelasPerSquareMeter
    def units = Set(CandelasPerSquareMeter)
  }


  object CandelasPerSquareMeter extends LuminanceUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "cd/m²"
  }

  object LuminanceConversions {
    lazy val candelaPerSquareMeter = CandelasPerSquareMeter(1)

    implicit class LuminanceConversions[A](a: A)(implicit n: Numeric[A]) {
      def candelasPerSquareMeter = CandelasPerSquareMeter(a)
    }
  }
}


