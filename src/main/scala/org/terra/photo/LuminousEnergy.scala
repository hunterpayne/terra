/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package photo

import scala.reflect.ClassTag

import time.TimeIntegral

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.standard.photo.LumenSeconds]]
 */
final class LuminousEnergyLike[C <: TypeContext](val value: C#T, val unit: LuminousEnergyUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[LuminousEnergyLike[C], C#T, C]
    with TimeIntegral[LuminousFluxLike[C], C#T, C#T, C] {

  import ops.luminousEnergyOps._
  import ops.timeOps.Seconds
  import ops.luminousFluxOps.Lumens

  def dimension: Dimension[LuminousEnergyLike[C], C#T, C] = LuminousEnergy

  protected def timeDerived = Lumens(toLumenSeconds)
  protected[terra] def time = Seconds(1)

  def toLumenSeconds = to(LumenSeconds)
}

trait LuminousEnergyUnit[C <: TypeContext] 
    extends UnitOfMeasure[LuminousEnergyLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new LuminousEnergyLike[C](t, this)
}

trait LuminousEnergyOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait LuminousEnergyUnitT extends LuminousEnergyUnit[C]

  object LuminousEnergy extends Dimension[LuminousEnergyLike[C], C#T, C] {
    private[photo] def apply[A](a: A, unit: LuminousEnergyUnit[C])(
      implicit n: Numeric[A]) = 
      new LuminousEnergyLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "LuminousEnergy"
    def primaryUnit = LumenSeconds
    def siUnit = LumenSeconds
    def units = Set(LumenSeconds)
  }

  object LumenSeconds extends LuminousEnergyUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "lm⋅s"
  }

  object LuminousEnergyConversions {
    lazy val lumenSecond = LumenSeconds(1)

    implicit class LuminousEnergyConversions[A](a: A)(implicit n: Numeric[A]) {
      def lumenSeconds = LumenSeconds(a)
    }
  }
}

