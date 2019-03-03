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
import time.TimeDerivative

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.standard.photo.Lumens]]
 */
final class LuminousFluxLike[C <: TypeContext](
  val value: C#T, val unit: LuminousFluxUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[LuminousFluxLike[C], C#T, C]
    with TimeDerivative[LuminousEnergyLike[C], C#T, C#T, C] {

  import ops.luminousFluxOps._
  import ops.timeOps.Seconds
  import ops.luminousEnergyOps.LumenSeconds
  import ops.illuminanceOps.Lux
  import ops.areaOps.SquareMeters
  import ops.luminousIntensityOps.Candelas
  import ops.solidAngleOps.SquareRadians

  type Area = AreaLike[C]
  type Illuminance = IlluminanceLike[C]
  type SolidAngle = SolidAngleLike[C]
  type LuminousIntensity = LuminousIntensityLike[C]

  def dimension: Dimension[LuminousFluxLike[C], C#T, C] = LuminousFlux

  protected[terra] def timeIntegrated = LumenSeconds(toLumens)
  protected[terra] def time = Seconds(1)

  def /(that: Area)(implicit ops: TerraOps[C]): Illuminance = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Lux(ops.div[C#T](this.toLumens, that.toSquareMeters))
  }
  def /(that: Illuminance)(implicit ops: TerraOps[C]): Area = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    SquareMeters(ops.div[C#T](this.toLumens, that.toLux))
  }
  def /(that: SolidAngle)(implicit ops: TerraOps[C]): LuminousIntensity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Candelas(ops.div[C#T](this.toLumens, that.toSquareRadians))
  }
  def /(that: LuminousIntensity)(implicit ops: TerraOps[C]): SolidAngle = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    SquareRadians(ops.div[C#T](this.toLumens, that.toCandelas))
  }

  def toLumens = to(Lumens)
}

trait LuminousFluxUnit[C <: TypeContext] 
    extends UnitOfMeasure[LuminousFluxLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new LuminousFluxLike[C](t, this)
}

trait LuminousFluxOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait LuminousFluxUnitT extends LuminousFluxUnit[C]

  object LuminousFlux extends Dimension[LuminousFluxLike[C], C#T, C] {
    private[photo] def apply[A](a: A, unit: LuminousFluxUnit[C])(
      implicit n: Numeric[A]) = 
      new LuminousFluxLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "LuminousFlux"
    def primaryUnit = Lumens
    def siUnit = Lumens
    def units = Set(Lumens)
  }


  object Lumens extends LuminousFluxUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "lm"
  }

  object LuminousFluxConversions {
    lazy val lumen = Lumens(1)

    implicit class LuminousFluxConversions[A](a: A)(implicit n: Numeric[A]) {
      def lumens = Lumens(a)
    }
  }
}

