
package org.terra
package energy

/**
  * Contains all the symbols exported by the org.terra.*.energy packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * @author Hunter Payne
  */
trait EnergySymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type Energy = EnergyLike[Tuple]
  lazy val WattHours = ops.energyOps.WattHours
  lazy val MilliwattHours = ops.energyOps.MilliwattHours
  lazy val KilowattHours = ops.energyOps.KilowattHours
  lazy val MegawattHours = ops.energyOps.MegawattHours
  lazy val GigawattHours = ops.energyOps.GigawattHours
  lazy val Joules = ops.energyOps.Joules
  lazy val Picojoules = ops.energyOps.Picojoules
  lazy val Nanojoules = ops.energyOps.Nanojoules
  lazy val Microjoules = ops.energyOps.Microjoules
  lazy val Millijoules = ops.energyOps.Millijoules
  lazy val Kilojoules = ops.energyOps.Kilojoules
  lazy val Megajoules = ops.energyOps.Megajoules
  lazy val Gigajoules = ops.energyOps.Gigajoules
  lazy val Terajoules = ops.energyOps.Terajoules
  lazy val BritishThermalUnits = ops.energyOps.BritishThermalUnits
  lazy val MBtus = ops.energyOps.MBtus
  lazy val MMBtus = ops.energyOps.MMBtus
  lazy val Ergs = ops.energyOps.Ergs
  lazy val ElectronVolt = ops.energyOps.ElectronVolt
  lazy val MilliElectronVolt = ops.energyOps.MilliElectronVolt
  lazy val KiloElectronVolt = ops.energyOps.KiloElectronVolt
  lazy val MegaElectronVolt = ops.energyOps.MegaElectronVolt
  lazy val GigaElectronVolt = ops.energyOps.GigaElectronVolt
  lazy val TeraElectronVolt = ops.energyOps.TeraElectronVolt
  lazy val PetaElectronVolt = ops.energyOps.PetaElectronVolt
  lazy val ExaElectronVolt = ops.energyOps.ExaElectronVolt
  lazy val Energy = ops.energyOps.Energy

  object EnergyConversions {
    import ops.energyOps.{ EnergyConversions => Convs }

    lazy val wattHour = Convs.wattHour
    lazy val Wh = Convs.Wh
    lazy val milliwattHour = Convs.milliwattHour
    lazy val mWh = Convs.mWh
    lazy val kilowattHour = Convs.kilowattHour
    lazy val kWh = Convs.kWh
    lazy val megawattHour = Convs.megawattHour
    lazy val MWh = Convs.MWh
    lazy val gigawattHour = Convs.gigawattHour
    lazy val GWh = Convs.GWh

    lazy val joule = Convs.joule
    lazy val picojoule = Convs.picojoule
    lazy val nanojoule = Convs.nanojoule
    lazy val microjoule = Convs.microjoule
    lazy val millijoule = Convs.millijoule
    lazy val kilojoule = Convs.kilojoule
    lazy val megajoule = Convs.megajoule
    lazy val gigajoule = Convs.gigajoule
    lazy val terajoule = Convs.terajoule

    lazy val btu = Convs.btu
    lazy val btuMultiplier = Convs.btuMultiplier

    lazy val eV = Convs.eV
    lazy val meV = Convs.meV
    lazy val keV = Convs.keV
    lazy val MeV = Convs.MeV
    lazy val GeV = Convs.GeV
    lazy val TeV = Convs.TeV
    lazy val PeV = Convs.PeV
    lazy val EeV = Convs.EeV

    implicit class EnergyConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.EnergyConversions[A](a)

    implicit class EnergyStringConversions(s: String)
        extends Convs.EnergyStringConversions(s)

    implicit object EnergyNumeric
        extends AbstractQuantityNumeric[EnergyLike[Tuple], Tuple](Energy)
  }

  type EnergyDensity = EnergyDensityLike[Tuple]
  lazy val JoulesPerCubicMeter = ops.energyDensityOps.JoulesPerCubicMeter
  lazy val EnergyDensity = ops.energyDensityOps.EnergyDensity

  object EnergyDensityConversions {
    import ops.energyDensityOps.{ EnergyDensityConversions => Convs }

    lazy val joulePerCubicMeter = Convs.joulePerCubicMeter

    implicit class EnergyDensityConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.EnergyDensityConversions[A](a)

    implicit object EnergyDensityNumeric
        extends AbstractQuantityNumeric[EnergyDensityLike[Tuple], Tuple](
      EnergyDensity)
  }

  type MolarEnergy = MolarEnergyLike[Tuple]
  lazy val JoulesPerMole = ops.molarEnergyOps.JoulesPerMole
  lazy val MolarEnergy = ops.molarEnergyOps.MolarEnergy

  object MolarEnergyConversions {
    import ops.molarEnergyOps.{ MolarEnergyConversions => Convs }

    lazy val joulePerMole = Convs.joulePerMole

    implicit class MolarEnergyConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.MolarEnergyConversions[A](a)

    implicit object MolarEnergyNumeric
        extends AbstractQuantityNumeric[MolarEnergyLike[Tuple], Tuple](
      MolarEnergy)
  }

  type Power = PowerLike[Tuple]
  lazy val Watts = ops.powerOps.Watts
  lazy val Milliwatts = ops.powerOps.Milliwatts
  lazy val Kilowatts = ops.powerOps.Kilowatts
  lazy val Megawatts = ops.powerOps.Megawatts
  lazy val Gigawatts = ops.powerOps.Gigawatts
  lazy val BtusPerHour = ops.powerOps.BtusPerHour
  lazy val ErgsPerSecond = ops.powerOps.ErgsPerSecond
  lazy val SolarLuminosities = ops.powerOps.SolarLuminosities
  lazy val Power = ops.powerOps.Power

  object PowerConversions {
    import ops.powerOps.{ PowerConversions => Convs }

    lazy val milliwatt = Convs.milliwatt
    lazy val mW = Convs.mW
    lazy val watt = Convs.watt
    lazy val W = Convs.W
    lazy val kilowatt = Convs.kilowatt
    lazy val kW = Convs.kW
    lazy val megawatt = Convs.megawatt
    lazy val MW = Convs.MW
    lazy val gigawatt = Convs.gigawatt
    lazy val GW = Convs.GW
    lazy val solarLuminosity = Convs.solarLuminosity

    implicit class PowerConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.PowerConversions[A](a)

    implicit class PowerStringConversions(s: String)
        extends Convs.PowerStringConversions(s)

    implicit object PowerNumeric
        extends AbstractQuantityNumeric[PowerLike[Tuple], Tuple](Power)
  }

  type PowerDensity = PowerDensityLike[Tuple]
  lazy val WattsPerCubicMeter = ops.powerDensityOps.WattsPerCubicMeter
  lazy val PowerDensity = ops.powerDensityOps.PowerDensity

  object PowerDensityConversions {
    import ops.powerDensityOps.{ PowerDensityConversions => Convs }

    lazy val wattPerCubicMeter = Convs.wattPerCubicMeter

    implicit class PowerDensityConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.PowerDensityConversions[A](a)

    implicit object PowerDensityNumeric
        extends AbstractQuantityNumeric[PowerDensityLike[Tuple], Tuple](
      PowerDensity)
  }

  type PowerRamp = PowerRampLike[Tuple]
  lazy val WattsPerHour = ops.powerRampOps.WattsPerHour
  lazy val WattsPerMinute = ops.powerRampOps.WattsPerMinute
  lazy val KilowattsPerHour = ops.powerRampOps.KilowattsPerHour
  lazy val KilowattsPerMinute = ops.powerRampOps.KilowattsPerMinute
  lazy val MegawattsPerHour = ops.powerRampOps.MegawattsPerHour
  lazy val GigawattsPerHour = ops.powerRampOps.GigawattsPerHour
  lazy val PowerRamp = ops.powerRampOps.PowerRamp

  object PowerRampConversions {
    import ops.powerRampOps.{ PowerRampConversions => Convs }

    lazy val wattPerHour = Convs.wattPerHour
    lazy val Wph = Convs.Wph
    lazy val wattPerMinute = Convs.wattPerMinute
    lazy val Wpm = Convs.Wpm
    lazy val kilowattPerHour = Convs.kilowattPerHour
    lazy val kWph = Convs.kWph
    lazy val kilowattPerMinute = Convs.kilowattPerMinute
    lazy val kWpm = Convs.kWpm
    lazy val megawattPerHour = Convs.megawattPerHour
    lazy val MWph = Convs.MWph
    lazy val gigawattPerHour = Convs.gigawattPerHour
    lazy val GWph = Convs.GWph

    implicit class PowerRampConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.PowerRampConversions[A](a)

    implicit class PowerRampStringConversion(s: String)
        extends Convs.PowerRampStringConversion(s)

    implicit object PowerRampNumeric
        extends AbstractQuantityNumeric[PowerRampLike[Tuple], Tuple](PowerRamp)
  }

  type SpecificEnergy = SpecificEnergyLike[Tuple]
  lazy val Grays = ops.specificEnergyOps.Grays
  lazy val Rads = ops.specificEnergyOps.Rads
  lazy val ErgsPerGram = ops.specificEnergyOps.ErgsPerGram
  lazy val SpecificEnergy = ops.specificEnergyOps.SpecificEnergy

  object SpecificEnergyConversions {
    import ops.specificEnergyOps.{ SpecificEnergyConversions => Convs }

    lazy val gray = Convs.gray
    lazy val rad = Convs.rad
    lazy val ergsPerGram = Convs.ergsPerGram

    implicit class SpecificEnergyConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.SpecificEnergyConversions[A](a)

    implicit object SpecificEnergyNumeric
        extends AbstractQuantityNumeric[SpecificEnergyLike[Tuple], Tuple](
      SpecificEnergy)
  }

  object KineticEnergy extends KineticEnergy[Tuple]
}
