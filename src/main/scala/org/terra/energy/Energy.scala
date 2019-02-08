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

import mass.{ MassLike, ChemicalAmountLike }
import motion.{ TorqueLike, ForceLike }
import thermal.{ ThermalCapacityLike, TemperatureLike }
import time._
import electro._
import radio.{ IrradianceLike, ParticleFluxLike }
import space.{ AreaLike, LengthLike, VolumeLike, AngleLike }

/**
 * Represents a quantity of energy
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.energy.WattHours]]
 */
final class EnergyLike[C <: TypeContext](
  val value: C#T, val unit: EnergyUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[EnergyLike[C], C#T, C]
    with TimeIntegral[PowerLike[C], C#T, C#T, C]
    with SecondTimeIntegral[PowerRampLike[C], C] {

  import ops.energyOps._
  import ops.powerOps.Watts
  import ops.timeOps.Hours
  import ops.electricPotentialOps.Volts
  import ops.electricChargeOps.Coulombs
  import ops.irradianceOps.WattsPerSquareMeter
  import ops.forceOps.Newtons
  import ops.lengthOps.Meters
  import ops.specificEnergyOps.Grays
  import ops.massOps.Kilograms
  import ops.energyDensityOps.JoulesPerCubicMeter
  import ops.volumeOps.CubicMeters
  import ops.thermalCapacityOps.JoulesPerKelvin
  import ops.temperatureOps.Kelvin
  import ops.molarEnergyOps.JoulesPerMole
  import ops.torqueOps.NewtonMeters

  type Area = AreaLike[C]
  type Angle = AngleLike[C]
  type Length = LengthLike[C]
  type Volume = VolumeLike[C]
  type TimeSquared = TimeSquaredLike[C]
  type Power = PowerLike[C]
  type PowerRamp = PowerRampLike[C]
  type ElectricCharge = ElectricChargeLike[C]
  type ElectricPotential = ElectricPotentialLike[C]
  type EnergyDensity = EnergyDensityLike[C]
  type Irradiance = IrradianceLike[C]
  type ParticleFlux = ParticleFluxLike[C]
  type Force = ForceLike[C]
  type SpecificEnergy = SpecificEnergyLike[C]
  type Mass = MassLike[C]
  type ThermalCapacity = ThermalCapacityLike[C]
  type Temperature = TemperatureLike[C]
  type Torque = TorqueLike[C]
  type MolarEnergy = MolarEnergyLike[C]
  type ChemicalAmount = ChemicalAmountLike[C]

  def dimension: Dimension[EnergyLike[C], C#T, C] = Energy

  protected def timeDerived = Watts(toWattHours)
  protected def time = Hours(1)

  def *(that: ParticleFlux)(implicit ops: TerraOps[C]): Irradiance = 
    WattsPerSquareMeter(
      ops.num.times(
        ops.num.times(ops.rconvT(Hours(1).toSeconds), this.toWattHours),
        that.toBecquerelsPerSquareMeterSecond))
  def /(that: Length)(implicit ops: TerraOps[C]): Force = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Newtons(ops.div[C#T](this.toJoules, that.toMeters))
  }
  def /(that: Force)(implicit ops: TerraOps[C]): Length = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Meters(ops.div[C#T](this.toJoules, that.toNewtons))
  }
  def /(that: Mass)(implicit ops: TerraOps[C]): SpecificEnergy = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Grays(ops.div[C#T](this.toJoules, that.toKilograms))
  }
  def /(that: SpecificEnergy)(implicit ops: TerraOps[C]): Mass = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Kilograms(ops.div[C#T](this.toJoules, that.toGrays))
  }
  def /(that: Volume)(implicit ops: TerraOps[C]): EnergyDensity = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    JoulesPerCubicMeter(ops.div[C#T](this.toJoules, that.toCubicMeters))
  }
  def /(that: EnergyDensity)(implicit ops: TerraOps[C]): Volume = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    CubicMeters(ops.div[C#T](this.toJoules, that.toJoulesPerCubicMeter))
  }

  def /(that: ElectricCharge)(implicit ops: TerraOps[C]): ElectricPotential = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Volts(ops.div[C#T](this.toJoules, that.toCoulombs))
  }
  def /(that: ElectricPotential)(implicit ops: TerraOps[C]): ElectricCharge = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Coulombs(ops.div[C#T](this.toJoules, that.toVolts))
  }

  def /(that: Temperature)(implicit ops: TerraOps[C]): ThermalCapacity = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    JoulesPerKelvin(ops.div[C#T](this.toJoules, that.toKelvinDegrees))
  }
  def /(that: ThermalCapacity)(implicit ops: TerraOps[C]) = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    Kelvin(ops.div[C#T](this.toJoules, that.toJoulesPerKelvin))
  }

  def /(that: ChemicalAmount)(implicit ops: TerraOps[C]): MolarEnergy = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    JoulesPerMole(ops.div[C#T](this.toJoules, that.toMoles))
  }
  def /(that: Angle)(implicit ops: TerraOps[C]): Torque = {
    implicit val ensureT: HasEnsureType[C#T] = ops.converters.ensureT
    NewtonMeters(ops.div[C#T](toJoules, that.toRadians))
  }
  def /(that: Area)(implicit ops: TerraOps[C]) = ??? // Insolation, Energy Area Density

  def /(that: TimeSquared)(implicit ops: TerraOps[C]): PowerRamp =
    this / that.time1 / that.time2
  def /(that: PowerRamp)(implicit ops: TerraOps[C]): TimeSquared =
    (this / that.timeIntegrated) * time

  def toWattHours = to(WattHours)
  def toMilliwattHours = to(MilliwattHours)
  def toKilowattHours = to(KilowattHours)
  def toMegawattHours = to(MegawattHours)
  def toGigawattHours = to(GigawattHours)

  def toJoules = to(Joules)
  def toPicojoules = to(Picojoules)
  def toNanojoules = to(Nanojoules)
  def toMicrojoules = to(Microjoules)
  def toMillijoules = to(Millijoules)
  def toKilojoules = to(Kilojoules)
  def toMegajoules = to(Megajoules)
  def toGigajoules = to(Gigajoules)
  def toTerajoules = to(Terajoules)

  def toeV = to(ElectronVolt)
  def tomeV = to(MilliElectronVolt)
  def tokeV = to(KiloElectronVolt)
  def toMeV = to(MegaElectronVolt)
  def toGeV = to(GigaElectronVolt)
  def toTeV = to(TeraElectronVolt)
  def toPeV = to(PetaElectronVolt)
  def toEeV = to(ExaElectronVolt)

  def toBtus = to(BritishThermalUnits)
  def toMBtus = to(MBtus)
  def toMMBtus = to(MMBtus)
  def toErgs = to(Ergs)

  /**
    * Energy and torque have the same unit, so convert appropriately
    * @return numerically equivalent value in newton-meters
    */
  def asTorque = ops.torqueOps.NewtonMeters(toJoules)
}

/**
 * Base trait for units of [[org.terra.energy.Energy]]
 */
trait EnergyUnit[C <: TypeContext] extends UnitOfMeasure[EnergyLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new EnergyLike[C](t, this)
}

trait EnergyOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait EnergyUnitT extends EnergyUnit[C]

  /**
    * Companion object for [[org.terra.energy.Energy]]
    */
  object Energy extends Dimension[EnergyLike[C], C#T, C] {
    private[energy] def apply[A](a: A, unit: EnergyUnit[C])(
      implicit n: Numeric[A]) = 
      new EnergyLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(load: PowerLike[C], time: TimeLike[C]): EnergyLike[C] = 
      load * time
    def apply(value: Any) = parse(value)

    def name = "Energy"
    def primaryUnit = WattHours
    def siUnit = Joules
    def units = Set(WattHours, MilliwattHours, KilowattHours, MegawattHours, GigawattHours,
      Joules, Picojoules, Nanojoules, Microjoules, Millijoules,
      Kilojoules, Megajoules, Gigajoules, Terajoules,
      BritishThermalUnits, MBtus, MMBtus, Ergs,
      ElectronVolt, MilliElectronVolt, KiloElectronVolt, MegaElectronVolt,
      GigaElectronVolt, TeraElectronVolt, PetaElectronVolt, ExaElectronVolt)
  }


  object WattHours extends EnergyUnitT with PrimaryUnit[C#T, C] {
    val symbol = "Wh"
  }

  import ops.powerOps.Watts
  import ops.timeOps.Time

  object MilliwattHours extends EnergyUnitT {
    val conversionFactor = Watts.conversionFactor * MetricSystem.Milli
    val symbol = "mWh"
  }

  object KilowattHours extends EnergyUnitT {
    val conversionFactor = Watts.conversionFactor * MetricSystem.Kilo
    val symbol = "kWh"
  }

  object MegawattHours extends EnergyUnitT {
    val conversionFactor = Watts.conversionFactor * MetricSystem.Mega
    val symbol = "MWh"
  }

  object GigawattHours extends EnergyUnitT {
    val conversionFactor = Watts.conversionFactor * MetricSystem.Giga
    val symbol = "GWh"
  }

  object Joules extends EnergyUnitT with SiUnit {
    val conversionFactor = 1.0 / Time.SecondsPerHour
    val symbol = "J"
  }

  object Picojoules extends EnergyUnitT with SiUnit {
    val conversionFactor = Joules.conversionFactor * MetricSystem.Pico
    val symbol = "pJ"
  }

  object Nanojoules extends EnergyUnitT with SiUnit {
    val conversionFactor = Joules.conversionFactor * MetricSystem.Nano
    val symbol = "nJ"
  }

  object Microjoules extends EnergyUnitT with SiUnit {
    val conversionFactor = Joules.conversionFactor * MetricSystem.Micro
    val symbol = "µJ"
  }

  object Millijoules extends EnergyUnitT with SiUnit {
    val conversionFactor = Joules.conversionFactor * MetricSystem.Milli
    val symbol = "mJ"
  }

  object Kilojoules extends EnergyUnitT with SiUnit {
    val conversionFactor = Joules.conversionFactor * MetricSystem.Kilo
    val symbol = "kJ"
  }

  object Megajoules extends EnergyUnitT with SiUnit {
    val conversionFactor = Joules.conversionFactor * MetricSystem.Mega
    val symbol = "MJ"
  }

  object Gigajoules extends EnergyUnitT with SiUnit {
    val conversionFactor = Joules.conversionFactor * MetricSystem.Giga
    val symbol = "GJ"
  }

  object Terajoules extends EnergyUnitT with SiUnit {
    val conversionFactor = Joules.conversionFactor * MetricSystem.Tera
    val symbol = "TJ"
  }

  object BritishThermalUnits extends EnergyUnitT {
    val conversionFactor = EnergyConversions.btuMultiplier
    val symbol = "Btu"
  }

  object MBtus extends EnergyUnitT {
    val conversionFactor = EnergyConversions.btuMultiplier * MetricSystem.Kilo
    val symbol = "MBtu"
  }

  object MMBtus extends EnergyUnitT {
    val conversionFactor = EnergyConversions.btuMultiplier * MetricSystem.Mega
    val symbol = "MMBtu"
  }

  object Ergs extends EnergyUnitT {
    val conversionFactor = 100.0 * Nanojoules.conversionFactor
    val symbol = "erg"
  }

  object ElectronVolt extends EnergyUnitT {
    val conversionFactor = Joules.conversionFactor * 1.602176565e-19
    val symbol = "eV"
  }

  object MilliElectronVolt extends EnergyUnitT {
    val conversionFactor = ElectronVolt.conversionFactor * MetricSystem.Milli
    val symbol = "meV"
  }

  object KiloElectronVolt extends EnergyUnitT {
    val conversionFactor = ElectronVolt.conversionFactor * MetricSystem.Kilo
    val symbol = "keV"
  }

  object MegaElectronVolt extends EnergyUnitT {
    val conversionFactor = ElectronVolt.conversionFactor * MetricSystem.Mega
    val symbol = "MeV"
  }

  object GigaElectronVolt extends EnergyUnitT {
    val conversionFactor = ElectronVolt.conversionFactor * MetricSystem.Giga
    val symbol = "GeV"
  }

  object TeraElectronVolt extends EnergyUnitT {
    val conversionFactor = ElectronVolt.conversionFactor * MetricSystem.Tera
    val symbol = "TeV"
  }

  object PetaElectronVolt extends EnergyUnitT {
    val conversionFactor = ElectronVolt.conversionFactor * MetricSystem.Peta
    val symbol = "PeV"
  }

  object ExaElectronVolt extends EnergyUnitT {
    val conversionFactor = ElectronVolt.conversionFactor * MetricSystem.Exa
    val symbol = "EeV"
  }

  object EnergyConversions {
    lazy val wattHour = WattHours(1)
    lazy val Wh = wattHour
    lazy val milliwattHour = MilliwattHours(1)
    lazy val mWh = milliwattHour
    lazy val kilowattHour = KilowattHours(1)
    lazy val kWh = kilowattHour
    lazy val megawattHour = MegawattHours(1)
    lazy val MWh = megawattHour
    lazy val gigawattHour = GigawattHours(1)
    lazy val GWh = gigawattHour

    lazy val joule = Joules(1)
    lazy val picojoule = Picojoules(1)
    lazy val nanojoule = Nanojoules(1)
    lazy val microjoule = Microjoules(1)
    lazy val millijoule = Millijoules(1)
    lazy val kilojoule = Kilojoules(1)
    lazy val megajoule = Megajoules(1)
    lazy val gigajoule = Gigajoules(1)
    lazy val terajoule = Terajoules(1)

    lazy val btu = BritishThermalUnits(1)
    lazy val btuMultiplier = 2.930710701722222e-1

    lazy val eV = ElectronVolt(1)
    lazy val meV = MilliElectronVolt(1)
    lazy val keV = KiloElectronVolt(1)
    lazy val MeV = MegaElectronVolt(1)
    lazy val GeV = GigaElectronVolt(1)
    lazy val TeV = TeraElectronVolt(1)
    lazy val PeV = PetaElectronVolt(1)
    lazy val EeV = ExaElectronVolt(1)

    implicit class EnergyConversions[A](a: A)(implicit num: Numeric[A]) {
      def J = Joules(a)
      def joules = Joules(a)
      def pJ = Picojoules(a)
      def picojoules = Picojoules(a)
      def nJ = Nanojoules(a)
      def nanojoules = Nanojoules(a)
      def µJ = Microjoules(a)
      def microjoules = Microjoules(a)
      def mJ = Millijoules(a)
      def milljoules = Millijoules(a)
      def kJ = Kilojoules(a)
      def kilojoules = Kilojoules(a)
      def MJ = Megajoules(a)
      def megajoules = Megajoules(a)
      def GJ = Gigajoules(a)
      def gigajoules = Gigajoules(a)
      def TJ = Terajoules(a)
      def terajoules = Terajoules(a)

      def Wh = WattHours(a)
      def mWh = MilliwattHours(a)
      def kWh = KilowattHours(a)
      def MWh = MegawattHours(a)
      def GWh = GigawattHours(a)
      def Btu = BritishThermalUnits(a)
      def MBtu = MBtus(a)
      def MMBtu = MMBtus(a)
      def ergs = Ergs(a)
      def wattHours = WattHours(a)
      def kilowattHours = KilowattHours(a)
      def megawattHours = MegawattHours(a)
      def gigawattHours = GigawattHours(a)

      def eV = ElectronVolt(a)
      def meV = MilliElectronVolt(a)
      def keV = KiloElectronVolt(a)
      def MeV = MegaElectronVolt(a)
      def GeV = GigaElectronVolt(a)
      def TeV = TeraElectronVolt(a)
      def PeV = PetaElectronVolt(a)
      def EeV = ExaElectronVolt(a)
    }

    implicit class EnergyStringConversions(s: String) {
      def toEnergy = Energy(s)
    }
  }
}
