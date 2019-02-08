/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package energy

import scala.reflect.ClassTag

import space.VolumeLike

/**
 * Represents a quantity of energy
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.standard.energy.WattHours]]
 */
final class EnergyDensityLike[C <: TypeContext](
  val value: C#T, val unit: EnergyDensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[EnergyDensityLike[C], C#T, C] {

  import ops.energyDensityOps._
  import ops.energyOps.Joules

  type Volume = VolumeLike[C]
  type Energy = EnergyLike[C]

  def dimension: Dimension[EnergyDensityLike[C], C#T, C] = EnergyDensity

  def *(that: Volume)(implicit ops: TerraOps[C]): Energy = 
    Joules(ops.num.times(this.toJoulesPerCubicMeter, that.toCubicMeters))

  def toJoulesPerCubicMeter = to(JoulesPerCubicMeter)
}

trait EnergyDensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[EnergyDensityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new EnergyDensityLike[C](t, this)
}

trait EnergyDensityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait EnergyDensityUnitT extends EnergyDensityUnit[C]

  object EnergyDensity extends Dimension[EnergyDensityLike[C], C#T, C] {
    private[energy] def apply[A](a: A, unit: EnergyDensityUnit[C])(
      implicit n: Numeric[A]) =
      new EnergyDensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "EnergyDensity"
    def primaryUnit = JoulesPerCubicMeter
    def siUnit = JoulesPerCubicMeter
    def units = Set(JoulesPerCubicMeter)
  }


  object JoulesPerCubicMeter extends EnergyDensityUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "j/m³"
  }

  object EnergyDensityConversions {
    lazy val joulePerCubicMeter = JoulesPerCubicMeter(1)

    implicit class EnergyDensityConversions[A](a: A)(implicit num: Numeric[A]) {
      def joulesPerCubicMeter = JoulesPerCubicMeter(a)
    }
  }
}

