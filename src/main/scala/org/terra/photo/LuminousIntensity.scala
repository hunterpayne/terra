/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package photo

import space.{ SolidAngleLike, AreaLike }

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.standard.photo.Candelas]]
 */
final class LuminousIntensityLike[C <: TypeContext](
  val value: C#T, val unit: LuminousIntensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[LuminousIntensityLike[C], C#T, C] {

  import ops.luminousIntensityOps._
  import ops.luminousFluxOps.Lumens
  import ops.luminanceOps.CandelasPerSquareMeter
  import ops.areaOps.SquareMeters

  type SolidAngle = SolidAngleLike[C]
  type LuminousFlux = LuminousFluxLike[C]
  type Area = AreaLike[C]
  type Luminance = LuminanceLike[C]

  def dimension: Dimension[LuminousIntensityLike[C], C#T, C] = LuminousIntensity

  def *(that: SolidAngle)(implicit ops: TerraOps[C]): LuminousFlux = 
    Lumens(ops.num.times(this.toCandelas, that.toSquareRadians))
  def /(that: Area)(implicit ops: TerraOps[C]): Luminance = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CandelasPerSquareMeter(ops.div[C#T](this.toCandelas, that.toSquareMeters))
  }
  def /(that: Luminance)(implicit ops: TerraOps[C]): Area = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    SquareMeters(ops.div[C#T](this.toCandelas, that.toCandelasPerSquareMeters))
  }

  def toCandelas = to(Candelas)
}

trait LuminousIntensityUnit[C <: TypeContext]
    extends UnitOfMeasure[LuminousIntensityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new LuminousIntensityLike[C](t, this)
}

trait LuminousIntensityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait LuminousIntensityUnitT extends LuminousIntensityUnit[C]

  object LuminousIntensity 
      extends Dimension[LuminousIntensityLike[C], C#T, C] with BaseDimension {
    private[photo] def apply[A](a: A, unit: LuminousIntensityUnit[C])(
      implicit n: Numeric[A]) = 
      new LuminousIntensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "LuminousIntensity"
    def primaryUnit = Candelas
    def units = Set(Candelas)
    def siUnit = Candelas
    def dimensionSymbol = "J"
  }

  object Candelas extends LuminousIntensityUnitT with PrimaryUnit[C#T, C]
      with SiBaseUnit {
    val symbol = "cd"
  }

  object LuminousIntensityConversions {
    lazy val candela = Candelas(1)

    implicit class LuminousIntensityConversions[A](a: A)(implicit n: Numeric[A]) {
      def candelas = Candelas(a)
    }
  }
}

