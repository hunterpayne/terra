/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package thermal

import scala.reflect.ClassTag

import energy.EnergyLike

/**
 * Represents the capacity of some substance or system to hold thermal energy.
 *
 * Also a representation of Entropy
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value the value in [[org.terra.thermal.JoulesPerKelvin]]
 */
final class ThermalCapacityLike[C <: TypeContext](
  val value: C#T, val unit: ThermalCapacityUnit[C])(implicit ops: TerraOps[C])
    extends Quantity[ThermalCapacityLike[C], C#T, C] {

  import ops.thermalCapacityOps._
  //import ops.temperatureOps.
  import ops.energyOps.Joules

  type Temperature = TemperatureLike[C]
  type Energy = EnergyLike[C]

  def dimension: Dimension[ThermalCapacityLike[C], C#T, C] = ThermalCapacity

  def *(that: Temperature)(implicit ops: TerraOps[C]): Energy = 
    Joules(ops.num.times(this.toJoulesPerKelvin, that.toKelvinScale))

  def toJoulesPerKelvin = to(JoulesPerKelvin)
}

trait ThermalCapacityUnit[C <: TypeContext] 
    extends UnitOfMeasure[ThermalCapacityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new ThermalCapacityLike[C](t, this)
}

trait ThermalCapacityOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait ThermalCapacityUnitT extends ThermalCapacityUnit[C]

  object ThermalCapacity extends Dimension[ThermalCapacityLike[C], C#T, C] {
    private[thermal] def apply[A](a: A, unit: ThermalCapacityUnit[C])(
      implicit n: Numeric[A], ops: TerraOps[C]) =
      new ThermalCapacityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ThermalCapacity"
    def primaryUnit = JoulesPerKelvin
    def siUnit = JoulesPerKelvin
    def units = Set(JoulesPerKelvin)
  }

  object JoulesPerKelvin extends ThermalCapacityUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    def symbol = "J/K"
  }

  object ThermalCapacityConversions {
    lazy val joulePerKelvin = JoulesPerKelvin(1)

    implicit class ThermalCapacityConversions[A](a: A)(implicit n: Numeric[A]) {
      def joulesPerKelvin = JoulesPerKelvin(a)
    }
  }
}
