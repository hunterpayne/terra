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

import radio.{ IrradianceLike, RadiantIntensityLike, SpectralPowerLike }
import space.{ SolidAngleLike, LengthLike, AreaLike, VolumeLike }
import time.{ TimeDerivative, TimeIntegral, TimeLike }
import electro.{ ElectricPotentialLike, ElectricCurrentLike }

/**
 * Represents a quantity of power / load, the rate at which energy produced or used
 *
 * The first time derivative of [[org.terra.energy.EnergyLike]]
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.standard.energy.Watts]]
 */
final class PowerLike[C <: TypeContext](val value: C#T, val unit: PowerUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[PowerLike[C], C#T, C]
    with TimeDerivative[EnergyLike[C], C#T, C#T, C]
    with TimeIntegral[PowerRampLike[C], C#T, C#T, C] {

  import ops.powerOps._
  import ops.energyOps.WattHours
  import ops.powerRampOps.WattsPerHour
  import ops.timeOps.Hours
  import ops.electricCurrentOps.Amperes
  import ops.electricPotentialOps.Volts
  import ops.spectralPowerOps.WattsPerMeter
  import ops.lengthOps.Meters
  import ops.irradianceOps.WattsPerSquareMeter
  import ops.areaOps.SquareMeters
  import ops.solidAngleOps.SquareRadians
  import ops.radiantIntensityOps.WattsPerSteradian
  import ops.powerDensityOps.WattsPerCubicMeter

  type ElectricPotential = ElectricPotentialLike[C]
  type ElectricCurrent = ElectricCurrentLike[C]
  type PowerDensity = PowerDensityLike[C]
  type Length = LengthLike[C]
  type SpectralPower = SpectralPowerLike[C]
  type Area = AreaLike[C]
  type Irradiance = IrradianceLike[C]
  type SolidAngle = SolidAngleLike[C]
  type RadiantIntensity = RadiantIntensityLike[C]
  type Volume = VolumeLike[C]

  def dimension: Dimension[PowerLike[C], C#T, C] = Power

  protected[terra] def timeIntegrated = WattHours(toWatts)
  protected def timeDerived = WattsPerHour(toWatts)
  protected[terra] def time = Hours(1)

  def /(that: Length)(implicit ops: TerraOps[C]): SpectralPower = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    WattsPerMeter(ops.div[C#T](this.toWatts, that.toMeters))
  }
  def /(that: SpectralPower)(implicit ops: TerraOps[C]): Length = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Meters(ops.div[C#T](this.toWatts, that.toWattsPerMeter))
  }
  def /(that: Area)(implicit ops: TerraOps[C]): Irradiance = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    WattsPerSquareMeter(ops.div[C#T](this.toWatts, that.toSquareMeters))
  }
  def /(that: Irradiance)(implicit ops: TerraOps[C]): Area = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    SquareMeters(ops.div[C#T](this.toWatts, that.toWattsPerSquareMeter))
  }
  def /(that: RadiantIntensity)(implicit ops: TerraOps[C]): SolidAngle = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    SquareRadians(ops.div[C#T](this.toWatts, that.toWattsPerSteradian))
  }
  def /(that: SolidAngle)(implicit ops: TerraOps[C]): RadiantIntensity = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    WattsPerSteradian(ops.div[C#T](this.toWatts, that.toSteradians))
  }

  def /(that: ElectricPotential)(implicit ops: TerraOps[C]): ElectricCurrent = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Amperes(ops.div[C#T](this.toWatts, that.toVolts))
  }
  def /(that: ElectricCurrent)(implicit ops: TerraOps[C]): ElectricPotential = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Volts(ops.div[C#T](this.toWatts, that.toAmperes))
  }
  def /(that: Volume)(implicit ops: TerraOps[C]): PowerDensity = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    WattsPerCubicMeter(ops.div[C#T](this.toWatts, that.toCubicMeters))
  }

  def toMilliwatts = to(Milliwatts)
  def toWatts = to(Watts)
  def toKilowatts = to(Kilowatts)
  def toMegawatts = to(Megawatts)
  def toGigawatts = to(Gigawatts)
  def toBtusPerHour = to(BtusPerHour)
  def toErgsPerSecond = to(ErgsPerSecond)
  def toSolarLuminosities = to(SolarLuminosities)
}

trait PowerUnit[C <: TypeContext] extends UnitOfMeasure[PowerLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new PowerLike[C](t, this)
}

trait PowerOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait PowerUnitT extends PowerUnit[C]

  /**
    * Companion object for [[org.terra.energy.Power]]
    */
  object Power extends Dimension[PowerLike[C], C#T, C] {
    private[energy] def apply[A](a: A, unit: PowerUnit[C])(
      implicit n: Numeric[A]) = 
      new PowerLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(energy: EnergyLike[C], time: TimeLike[C]): PowerLike[C] = {
      implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
      implicit val tag: ClassTag[C#T] = ops.getClassTagT
      new PowerLike[C](
        ops.div[C#T](energy.toWattHours, ops.rconvT(time.toHours)), Watts)
    }
    def apply(value: Any) = parse(value)

    def name = "Power"
    def primaryUnit = Watts
    def siUnit = Watts
    def units = Set(Watts, Milliwatts, Kilowatts, Megawatts, Gigawatts, BtusPerHour, ErgsPerSecond, SolarLuminosities)
  }


  object Milliwatts extends PowerUnitT with SiUnit {
    val conversionFactor = MetricSystem.Milli
    val symbol = "mW"
  }

  object Watts extends PowerUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "W"
  }

  object Kilowatts extends PowerUnitT with SiUnit {
    val conversionFactor = MetricSystem.Kilo
    val symbol = "kW"
  }

  object Megawatts extends PowerUnitT with SiUnit {
    val conversionFactor = MetricSystem.Mega
    val symbol = "MW"
  }

  object Gigawatts extends PowerUnitT with SiUnit {
    val conversionFactor = MetricSystem.Giga
    val symbol = "GW"
  }

  object BtusPerHour extends PowerUnitT {
    val conversionFactor = ops.energyOps.EnergyConversions.btuMultiplier
    val symbol = "Btu/hr"
  }

  object ErgsPerSecond extends PowerUnitT {
    val conversionFactor = 1e-7
    val symbol = ops.energyOps.Ergs.symbol + "/" + ops.timeOps.Seconds.symbol
  }

  object SolarLuminosities extends PowerUnitT {
    val conversionFactor = 3.828e26
    val symbol = "L☉"
  }

  object PowerConversions {
    lazy val milliwatt = Milliwatts(1)
    lazy val mW = milliwatt
    lazy val watt = Watts(1)
    lazy val W = watt
    lazy val kilowatt = Kilowatts(1)
    lazy val kW = kilowatt
    lazy val megawatt = Megawatts(1)
    lazy val MW = megawatt
    lazy val gigawatt = Gigawatts(1)
    lazy val GW = gigawatt
    lazy val solarLuminosity = SolarLuminosities(1)

    implicit class PowerConversions[A](a: A)(implicit num: Numeric[A]) {
      def mW = Milliwatts(a)
      def W = Watts(a)
      def kW = Kilowatts(a)
      def MW = Megawatts(a)
      def GW = Gigawatts(a)
      def milliwatts = Milliwatts(a)
      def watts = Watts(a)
      def kilowatts = Kilowatts(a)
      def megawatts = Megawatts(a)
      def gigawatts = Gigawatts(a)
      def BTUph = BtusPerHour(a)
      def ergsPerSecond = ErgsPerSecond(a)
      def solarLuminosities = SolarLuminosities(a)
    }

    implicit class PowerStringConversions(s: String) {
      def toPower = Power(s)
    }
  }
}

