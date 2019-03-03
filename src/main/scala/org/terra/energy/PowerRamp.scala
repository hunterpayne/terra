/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package energy

import time._

/**
 * Represents the rate of change of [[org.terra.energy.PowerLike]] over time
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.standard.energy.WattsPerHour]]
 */
final class PowerRampLike[C <: TypeContext](
  val value: C#T, val unit: PowerRampUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[PowerRampLike[C], C#T, C]
    with TimeDerivative[PowerLike[C], C#T, C#T, C]
    with SecondTimeDerivative[EnergyLike[C], C] {

  import ops.powerRampOps._
  import ops.powerOps.Watts
  import ops.timeOps.Hours

  type Energy = EnergyLike[C]

  def dimension: Dimension[PowerRampLike[C], C#T, C] = PowerRamp

  protected[terra] def timeIntegrated = Watts(toWattsPerHour)
  protected[terra] def time = Hours(1)

  def *(that: TimeSquared)(implicit ops: TerraOps[C]): Energy = 
    this * that.time1 * that.time2

  def toWattsPerHour = to(WattsPerHour)
  def toWattsPerMinutes = to(WattsPerMinute)
  def toKilowattsPerHour = to(KilowattsPerHour)
  def toKilowattsPerMinute = to(KilowattsPerMinute)
  def toMegawattsPerHour = to(MegawattsPerHour)
  def toGigawattsPerHour = to(GigawattsPerHour)
}

trait PowerRampUnit[C <: TypeContext] 
    extends UnitOfMeasure[PowerRampLike[C], C#T, C] with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new PowerRampLike[C](t, this)
}

trait PowerRampOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait PowerRampUnitT extends PowerRampUnit[C]

  object PowerRamp extends Dimension[PowerRampLike[C], C#T, C] {
    private[energy] def apply[A](a: A, unit: PowerRampUnit[C])(
      implicit n: Numeric[A]) =
      new PowerRampLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(change: PowerLike[C], time: TimeLike[C]): 
        PowerRampLike[C] = {
      implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: PseudoClassTag[C#T] = ops.getClassTagT
      implicit val n: Numeric[C#T] = ops.num
      apply(ops.div[C#T](change.toWatts, ops.rconvT(time.toHours)), WattsPerHour)
    }
    def apply(value: Any) = parse(value)
    def name = "PowerRamp"
    def primaryUnit = WattsPerHour
    def siUnit = WattsPerHour
    def units = Set(WattsPerHour, WattsPerMinute, KilowattsPerHour, KilowattsPerMinute, MegawattsPerHour, GigawattsPerHour)
  }


  object WattsPerHour extends PowerRampUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = "W/h"
}

  object WattsPerMinute extends PowerRampUnitT with SiUnit {
    val conversionFactor = WattsPerHour.conversionFactor / 60D
    val symbol = "W/m"
  }

  object KilowattsPerHour extends PowerRampUnitT with SiUnit {
    val conversionFactor = MetricSystem.Kilo
    val symbol = "kW/h"
  }

  object KilowattsPerMinute extends PowerRampUnitT with SiUnit {
    val conversionFactor = KilowattsPerHour.conversionFactor / 60D
    val symbol = "kW/m"
  }

  object MegawattsPerHour extends PowerRampUnitT with SiUnit {
    val conversionFactor = MetricSystem.Mega
    val symbol = "MW/h"
  }

  object GigawattsPerHour extends PowerRampUnitT with SiUnit {
    val conversionFactor = MetricSystem.Giga
    val symbol = "GW/h"
  }

  object PowerRampConversions {
    lazy val wattPerHour = WattsPerHour(1)
    lazy val Wph = wattPerHour
    lazy val wattPerMinute = WattsPerMinute(1)
    lazy val Wpm = wattPerMinute
    lazy val kilowattPerHour = KilowattsPerHour(1)
    lazy val kWph = kilowattPerHour
    lazy val kilowattPerMinute = KilowattsPerMinute(1)
    lazy val kWpm = kilowattPerMinute
    lazy val megawattPerHour = MegawattsPerHour(1)
    lazy val MWph = megawattPerHour
    lazy val gigawattPerHour = GigawattsPerHour(1)
    lazy val GWph = gigawattPerHour

    implicit class PowerRampConversions[A](a: A)(implicit num: Numeric[A]) {
      def Wph = WattsPerHour(a)
      def Wpm = WattsPerMinute(a)
      def kWph = KilowattsPerHour(a)
      def kWpm = KilowattsPerMinute(a)
      def MWph = MegawattsPerHour(a)
      def GWph = GigawattsPerHour(a)
    }

    implicit class PowerRampStringConversion(s: String) {
      def toPowerRamp = PowerRamp(s)
    }
  }
}
