/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package energy

import space.VolumeLike

/**
  * Represents a quantity of power density
  *
  * @author  Nicolas Vinuesa
  * @since   1.4
  *
  * @param value value in [[org.terra.standard.energy.WattsPerCubicMeter]]
  */
final class PowerDensityLike[C <: TypeContext](
  val value: C#T, val unit: PowerDensityUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[PowerDensityLike[C], C#T, C] {

  import ops.powerDensityOps._
  import ops.powerOps.Watts

  type Volume = VolumeLike[C]
  type Power = PowerLike[C]

  def dimension: Dimension[PowerDensityLike[C], C#T, C] = PowerDensity

  def *(that: Volume)(implicit ops: TerraOps[C]): Power = 
    Watts(ops.num.times(this.toWattsPerCubicMeter, that.toCubicMeters))

  def toWattsPerCubicMeter = to(WattsPerCubicMeter)
}

trait PowerDensityUnit[C <: TypeContext] 
    extends UnitOfMeasure[PowerDensityLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new PowerDensityLike(t, this)
}

trait PowerDensityOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait PowerDensityUnitT extends PowerDensityUnit[C]

  object PowerDensity extends Dimension[PowerDensityLike[C], C#T, C] {
    private[energy] def apply[A](a: A, unit: PowerDensityUnit[C])(
      implicit n: Numeric[A]) = 
      new PowerDensityLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "PowerDensity"
    def primaryUnit = WattsPerCubicMeter
    def siUnit = WattsPerCubicMeter
    def units = Set(WattsPerCubicMeter)
  }


  object WattsPerCubicMeter extends PowerDensityUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "W/m³"
  }

  object PowerDensityConversions {
    lazy val wattPerCubicMeter = WattsPerCubicMeter(1)

    implicit class PowerDensityConversions[A](a: A)(implicit num: Numeric[A]) {
      def wattsPerCubicMeter = WattsPerCubicMeter(a)
    }
  }
}

