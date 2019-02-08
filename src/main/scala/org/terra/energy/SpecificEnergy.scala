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
 * @param value value in [[org.terra.energy.Grays]]
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
    Joules(ops.num.times(this.toGrays, that.toKilograms))
  def /(that: Time) = ??? // returns AbsorbedEnergyRate

  def toGrays = to(Grays)
  def toRads = to(Rads)
  def toErgsPerGram = to(ErgsPerGram)
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
    def primaryUnit = Grays
    def siUnit = Grays
    def units = Set(Grays, Rads, ErgsPerGram)
  }

  object Grays extends SpecificEnergyUnitT with PrimaryUnit[C#T, C] 
      with SiUnit {
    val symbol = "Gy"
  }

  object Rads extends SpecificEnergyUnitT with UnitConverter[C#T, C] {
    val symbol = "rad"
    val conversionFactor = 0.01
  }

  object ErgsPerGram extends SpecificEnergyUnitT with UnitConverter[C#T, C] {
    val symbol = "erg/g"
    val conversionFactor = 0.0001
  }

  object SpecificEnergyConversions {
    lazy val gray = Grays(1)
    lazy val rad = Rads(1)
    lazy val ergsPerGram = ErgsPerGram(1)

    implicit class SpecificEnergyConversions[A](a: A)(implicit num: Numeric[A]) {
      def grays = Grays(a)
      def rads = Rads(a)
      def ergsPerGram = ErgsPerGram(a)
    }
  }
}

