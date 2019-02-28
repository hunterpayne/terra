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

import time.TimeLike
import mass.MassLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.energy.JoulesPerKilogram]]
 */
final class SpecificEnergyLike[C <: TypeContext](
  val value: C#T, val unit: SpecificEnergyUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[SpecificEnergyLike[C], C#T, C] {

  import ops.specificEnergyOps._
  import ops.energyOps.Joules
  import ops.massOps.Mass
  import ops.timeOps.Time

  type Mass = MassLike[C]
  type Energy = EnergyLike[C]
  type Time = TimeLike[C]

  def dimension: Dimension[SpecificEnergyLike[C], C#T, C] = SpecificEnergy

  def *(that: Mass)(implicit ops: TerraOps[C]): Energy =
    Joules(ops.num.times(this.toJoulesPerKilogram, that.toKilograms))

  def toJoulesPerKilogram = to(JoulesPerKilogram)
}

trait SpecificEnergyUnit[C <: TypeContext] 
    extends UnitOfMeasure[SpecificEnergyLike[C], C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new SpecificEnergyLike[C](t, this)
}

trait SpecificEnergyOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait SpecificEnergyUnitT extends SpecificEnergyUnit[C]

  object SpecificEnergy extends Dimension[SpecificEnergyLike[C], C#T, C] {
    private[energy] def apply[A](a: A, unit: SpecificEnergyUnit[C])(
      implicit n: Numeric[A]) = 
      new SpecificEnergyLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "SpecificEnergy"
    def primaryUnit = JoulesPerKilogram
    def siUnit = JoulesPerKilogram
    def units = Set(JoulesPerKilogram)
  }

  object JoulesPerKilogram extends SpecificEnergyUnitT with PrimaryUnit[C#T, C] 
      with SiUnit {
    val symbol = 
      ops.energyOps.Joules.symbol + "/" + ops.massOps.Kilograms.symbol
  }

  object SpecificEnergyConversions {
    lazy val joulePerKilogram = JoulesPerKilogram(1)

    implicit class SpecificEnergyConversions[A](a: A)(implicit num: Numeric[A]) {
      def joulesPerKilogram = JoulesPerKilogram(a)
    }
  }
}

