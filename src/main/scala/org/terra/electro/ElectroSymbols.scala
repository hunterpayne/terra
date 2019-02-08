
package org.terra
package electro

/**
  * Contains all the symbols exported by the org.terra.*.electro packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * @author Hunter Payne
  */
trait ElectroSymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type AreaElectricChargeDensity = AreaElectricChargeDensityLike[Tuple]
  lazy val CoulombsPerSquareMeter =
    ops.areaElectricChargeDensityOps.CoulombsPerSquareMeter
  lazy val AreaElectricChargeDensity =
    ops.areaElectricChargeDensityOps.AreaElectricChargeDensity

  object AreaElectricChargeDensityConversions {

    import ops.areaElectricChargeDensityOps.{ AreaElectricChargeDensityConversions => Convs }

    lazy val coulombPerSquareMeter = Convs.coulombPerSquareMeter
    implicit class AreaElectricChargeDensityConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.AreaElectricChargeDensityConversions[A](a)

    implicit object AreaElectricChargeDensityNumeric
        extends AbstractQuantityNumeric[AreaElectricChargeDensityLike[Tuple], Tuple](
      AreaElectricChargeDensity)
  }

  type Capacitance = CapacitanceLike[Tuple]
  lazy val Farads = ops.capacitanceOps.Farads
  lazy val Picofarads = ops.capacitanceOps.Picofarads
  lazy val Nanofarads = ops.capacitanceOps.Nanofarads
  lazy val Microfarads = ops.capacitanceOps.Microfarads
  lazy val Millifarads = ops.capacitanceOps.Millifarads
  lazy val Kilofarads = ops.capacitanceOps.Kilofarads
  lazy val Capacitance = ops.capacitanceOps.Capacitance

  object CapacitanceConversions {

    import ops.capacitanceOps.{ CapacitanceConversions => Convs }

    lazy val farad = Convs.farad
    lazy val picofarad = Convs.picofarad
    lazy val nanofarad = Convs.nanofarad
    lazy val microfarad = Convs.microfarad
    lazy val millifarad = Convs.millifarad
    lazy val kilofarad = Convs.kilofarad

    implicit class CapacitanceConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.CapacitanceConversions[A](a)

    implicit object CapacitanceNumeric extends AbstractQuantityNumeric[CapacitanceLike[Tuple], Tuple](Capacitance)
  }

  type Conductivity = ConductivityLike[Tuple]
  lazy val SiemensPerMeter = ops.conductivityOps.SiemensPerMeter
  lazy val Conductivity = ops.conductivityOps.Conductivity

  object ConductivityConversions {

    import ops.conductivityOps.{ ConductivityConversions => Convs }
    lazy val siemenPerMeter = Convs.siemenPerMeter

    implicit class ConductivityConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.ConductivityConversions[A](a)

    implicit object ConductivityNumeric
        extends AbstractQuantityNumeric[ConductivityLike[Tuple], Tuple](
      Conductivity)
  }

  type ElectricCharge = ElectricChargeLike[Tuple]
  lazy val Coulombs = ops.electricChargeOps.Coulombs
  lazy val Picocoulombs = ops.electricChargeOps.Picocoulombs
  lazy val Nanocoulombs = ops.electricChargeOps.Nanocoulombs
  lazy val Microcoulombs = ops.electricChargeOps.Microcoulombs
  lazy val Millicoulombs = ops.electricChargeOps.Millicoulombs
  lazy val Abcoulombs = ops.electricChargeOps.Abcoulombs
  lazy val AmpereHours = ops.electricChargeOps.AmpereHours
  lazy val MilliampereHours = ops.electricChargeOps.MilliampereHours
  lazy val MilliampereSeconds = ops.electricChargeOps.MilliampereSeconds
  lazy val ElectricCharge = ops.electricChargeOps.ElectricCharge

  object ElectricChargeConversions {
    import ops.electricChargeOps.{ ElectricChargeConversions => Convs }

    lazy val coulomb = Convs.coulomb
    lazy val picocoulomb = Convs.picocoulomb
    lazy val nanocoulomb = Convs.nanocoulomb
    lazy val microcoulomb = Convs.microcoulomb
    lazy val millicoulomb = Convs.millicoulomb
    lazy val abcoulomb = Convs.abcoulomb
    lazy val ampereHour = Convs.ampereHour
    lazy val milliampereHour = Convs.milliampereHour
    lazy val milliampereSecond = Convs.milliampereSecond

    implicit class ElectricChargeConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.ElectricChargeConversions[A](a)

    implicit object ElectricalChargeNumeric
        extends AbstractQuantityNumeric[ElectricChargeLike[Tuple], Tuple](
      ElectricCharge)
  }

  type ElectricChargeDensity = ElectricChargeDensityLike[Tuple]
  lazy val CoulombsPerCubicMeter =
    ops.electricChargeDensityOps.CoulombsPerCubicMeter
  lazy val ElectricChargeDensity =
    ops.electricChargeDensityOps.ElectricChargeDensity

  object ElectricChargeDensityConversions {
    import ops.electricChargeDensityOps.{ ElectricChargeDensityConversions => Convs }

    lazy val coulombPerCubicMeter = Convs.coulombPerCubicMeter

    implicit class ElectricChargeDensityConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.ElectricChargeDensityConversions[A](a)

    implicit object ElectricChargeDensityNumeric
        extends AbstractQuantityNumeric[ElectricChargeDensityLike[Tuple], Tuple](
      ElectricChargeDensity)
  }

  type ElectricChargeMassRatio = ElectricChargeMassRatioLike[Tuple]
  lazy val CoulombsPerKilogram =
    ops.electricChargeMassRatioOps.CoulombsPerKilogram
  lazy val ElectricChargeMassRatio =
    ops.electricChargeMassRatioOps.ElectricChargeMassRatio

  object ElectricChargeMassRatioConversions {
    import ops.electricChargeMassRatioOps.{ ElectricChargeMassRatioConversions => Convs }

    lazy val coulombPerKilogram = Convs.coulombPerKilogram

    implicit class ElectricChargeMassRatioConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.ElectricChargeMassRatioConversions[A](a)

    implicit object ElectricChargeMassRatioNumeric
        extends AbstractQuantityNumeric[ElectricChargeMassRatioLike[Tuple], Tuple](
      ElectricChargeMassRatio)
  }

  type ElectricCurrent = ElectricCurrentLike[Tuple]
  lazy val Amperes = ops.electricCurrentOps.Amperes
  lazy val Milliamperes = ops.electricCurrentOps.Milliamperes
  lazy val ElectricCurrent = ops.electricCurrentOps.ElectricCurrent

  object ElectricCurrentConversions {
    import ops.electricCurrentOps.{ ElectricCurrentConversions => Convs }

    lazy val ampere = Convs.ampere
    lazy val amp = Convs.amp
    lazy val milliampere = Convs.milliampere
    lazy val milliamp = Convs.milliamp

    implicit class ElectricCurrentConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.ElectricCurrentConversions[A](a)

    implicit object ElectricCurrentNumeric
        extends AbstractQuantityNumeric[ElectricCurrentLike[Tuple], Tuple](
      ElectricCurrent)
  }

  type ElectricCurrentDensity = ElectricCurrentDensityLike[Tuple]
  lazy val AmperesPerSquareMeter =
    ops.electricCurrentDensityOps.AmperesPerSquareMeter
  lazy val ElectricCurrentDensity =
    ops.electricCurrentDensityOps.ElectricCurrentDensity

  object ElectricCurrentDensityConversions {
    import ops.electricCurrentDensityOps.{ ElectricCurrentDensityConversions => Convs }
    lazy val amperePerSquareMeter = Convs.amperePerSquareMeter

    implicit class ElectricCurrentDensityConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.ElectricCurrentDensityConversions[A](a)

    implicit object ElectricCurrentDensityNumeric
        extends AbstractQuantityNumeric[ElectricCurrentDensityLike[Tuple], Tuple](
      ElectricCurrentDensity)
  }

  type ElectricFieldStrength = ElectricFieldStrengthLike[Tuple]
  lazy val VoltsPerMeter = ops.electricFieldStrengthOps.VoltsPerMeter
  lazy val ElectricFieldStrength =
    ops.electricFieldStrengthOps.ElectricFieldStrength

  object ElectricFieldStrengthConversions {
    import ops.electricFieldStrengthOps.{ ElectricFieldStrengthConversions => Convs }
    lazy val voltPerMeter = Convs.voltPerMeter

    implicit class ElectricFieldStrengthConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.ElectricFieldStrengthConversions[A](a)

    implicit object ElectricFieldStrengthNumeric
        extends AbstractQuantityNumeric[ElectricFieldStrengthLike[Tuple], Tuple](
      ElectricFieldStrength)
  }

  type ElectricPotential = ElectricPotentialLike[Tuple]
  lazy val Volts = ops.electricPotentialOps.Volts
  lazy val Microvolts = ops.electricPotentialOps.Microvolts
  lazy val Millivolts = ops.electricPotentialOps.Millivolts
  lazy val Kilovolts = ops.electricPotentialOps.Kilovolts
  lazy val Megavolts = ops.electricPotentialOps.Megavolts
  lazy val ElectricPotential = ops.electricPotentialOps.ElectricPotential

  object ElectricPotentialConversions {
    import ops.electricPotentialOps.{ ElectricPotentialConversions => Convs }

    lazy val volt = Convs.volt
    lazy val microvolt = Convs.microvolt
    lazy val millivolt = Convs.millivolt
    lazy val kilovolt = Convs.kilovolt
    lazy val megavolt = Convs.megavolt

    implicit class ElectricPotentialConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.ElectricPotentialConversions[A](a)

    implicit object ElectricPotentialNumeric
        extends AbstractQuantityNumeric[ElectricPotentialLike[Tuple], Tuple](
      ElectricPotential)
  }

  type ElectricalConductance = ElectricalConductanceLike[Tuple]
  lazy val Siemens = ops.electricalConductanceOps.Siemens
  lazy val ElectricalConductance =
    ops.electricalConductanceOps.ElectricalConductance

  object ElectricalConductanceConversions {
    import ops.electricalConductanceOps.{ ElectricalConductanceConversions => Convs }
    lazy val siemen = Convs.siemen

    implicit class ElectricalConductanceConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.ElectricalConductanceConversions[A](a)

    implicit object ElectricalConductanceNumeric
        extends AbstractQuantityNumeric[ElectricalConductanceLike[Tuple], Tuple](
      ElectricalConductance)
  }

  type ElectricalResistance = ElectricalResistanceLike[Tuple]
  lazy val Ohms = ops.electricalResistanceOps.Ohms
  lazy val Nanohms = ops.electricalResistanceOps.Nanohms
  lazy val Microohms = ops.electricalResistanceOps.Microohms
  lazy val Milliohms = ops.electricalResistanceOps.Milliohms
  lazy val Kilohms = ops.electricalResistanceOps.Kilohms
  lazy val Megohms = ops.electricalResistanceOps.Megohms
  lazy val Gigohms = ops.electricalResistanceOps.Gigohms
  lazy val ElectricalResistance =
    ops.electricalResistanceOps.ElectricalResistance

  object ElectricalResistanceConversions {
    import ops.electricalResistanceOps.{ ElectricalResistanceConversions => Convs }
    lazy val ohm = Convs.ohm
    lazy val nanohm = Convs.nanohm
    lazy val microohm = Convs.microohm
    lazy val milliohm = Convs.milliohm
    lazy val kilohm = Convs.kilohm
    lazy val megohm = Convs.megohm
    lazy val gigohm = Convs.gigohm

    implicit class ElectricalResistanceConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.ElectricalResistanceConversions[A](a)

    implicit object ElectricalResistanceNumeric
        extends AbstractQuantityNumeric[ElectricalResistanceLike[Tuple], Tuple](
      ElectricalResistance)
  }

  type Inductance = InductanceLike[Tuple]
  lazy val Henry = ops.inductanceOps.Henry
  lazy val Millihenry = ops.inductanceOps.Millihenry
  lazy val Microhenry = ops.inductanceOps.Microhenry
  lazy val Nanohenry = ops.inductanceOps.Nanohenry
  lazy val Picohenry = ops.inductanceOps.Picohenry
  lazy val Inductance = ops.inductanceOps.Inductance

  object InductanceConversions {
    import ops.inductanceOps.{ InductanceConversions => Convs }

    lazy val henry = Convs.henry
    lazy val millihenry = Convs.millihenry
    lazy val microhenry = Convs.microhenry
    lazy val nanohenry = Convs.nanohenry
    lazy val picohenry = Convs.picohenry

    implicit class InductanceConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.InductanceConversions[A](a)

    implicit object InductanceNumeric
        extends AbstractQuantityNumeric[InductanceLike[Tuple], Tuple](
      Inductance)
  }

  type LinearElectricChargeDensity = LinearElectricChargeDensityLike[Tuple]
  lazy val CoulombsPerMeter =
    ops.linearElectricChargeDensityOps.CoulombsPerMeter
  lazy val LinearElectricChargeDensity =
    ops.linearElectricChargeDensityOps.LinearElectricChargeDensity

  object LinearElectricChargeDensityConversions {
    import ops.linearElectricChargeDensityOps.{ LinearElectricChargeDensityConversions => Convs }
    lazy val coulombPerMeter = Convs.coulombPerMeter

    implicit class LinearElectricChargeDensityConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.LinearElectricChargeDensityConversions[A](a)

    implicit object LinearElectricChargeDensityNumeric
        extends AbstractQuantityNumeric[LinearElectricChargeDensityLike[Tuple], Tuple](
      LinearElectricChargeDensity)
  }

  type MagneticFieldStrength = MagneticFieldStrengthLike[Tuple]
  lazy val AmperesPerMeter = ops.magneticFieldStrengthOps.AmperesPerMeter
  lazy val MagneticFieldStrength =
    ops.magneticFieldStrengthOps.MagneticFieldStrength

  object MagneticFieldStrengthConversions {
    import ops.magneticFieldStrengthOps.{ MagneticFieldStrengthConversions => Convs }
    lazy val amperePerMeter = Convs.amperePerMeter

    implicit class MagneticFieldStrengthConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.MagneticFieldStrengthConversions[A](a)

    implicit object MagneticFieldStrengthNumeric
        extends AbstractQuantityNumeric[MagneticFieldStrengthLike[Tuple], Tuple](
      MagneticFieldStrength)
  }

  type MagneticFlux = MagneticFluxLike[Tuple]
  lazy val Webers = ops.magneticFluxOps.Webers
  lazy val MagneticFlux = ops.magneticFluxOps.MagneticFlux

  object MagneticFluxConversions {
    import ops.magneticFluxOps.{ MagneticFluxConversions => Convs }

    lazy val weber = Convs.weber

    implicit class MagneticFluxConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.MagneticFluxConversions[A](a)

    implicit object MagneticFluxNumeric
        extends AbstractQuantityNumeric[MagneticFluxLike[Tuple], Tuple](
      MagneticFlux)
  }

  type MagneticFluxDensity = MagneticFluxDensityLike[Tuple]
  lazy val Teslas = ops.magneticFluxDensityOps.Teslas
  lazy val Gauss = ops.magneticFluxDensityOps.Gauss
  lazy val MagneticFluxDensity =
    ops.magneticFluxDensityOps.MagneticFluxDensity

  object MagneticFluxDensityConversions {
    import ops.magneticFluxDensityOps.{ MagneticFluxDensityConversions => Convs }
    lazy val tesla = Convs.tesla
    lazy val gauss = Convs.gauss

    implicit class MagneticFluxDensityConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.MagneticFluxDensityConversions[A](a)

    implicit object MagneticFluxDensistyNumeric
        extends AbstractQuantityNumeric[MagneticFluxDensityLike[Tuple], Tuple](
      MagneticFluxDensity)
  }

  type Permeability = PermeabilityLike[Tuple]
  lazy val HenriesPerMeter = ops.permeabilityOps.HenriesPerMeter
  lazy val NewtonsPerAmperesSquared =
    ops.permeabilityOps.NewtonsPerAmperesSquared
  lazy val Permeability = ops.permeabilityOps.Permeability

  object PermeabilityConversions {
    import ops.permeabilityOps.{ PermeabilityConversions => Convs }

    lazy val henriesPerMeter = Convs.henriesPerMeter
    lazy val newtonsPerAmperesSquared = Convs.newtonsPerAmperesSquared

    implicit class PermeabilityConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.PermeabilityConversions[A](a)

    implicit object PermeabilityNumeric
        extends AbstractQuantityNumeric[PermeabilityLike[Tuple], Tuple](
      Permeability)
  }

  type Permittivity = PermittivityLike[Tuple]
  lazy val FaradsPerMeter = ops.permittivityOps.FaradsPerMeter
  lazy val Permittivity = ops.permittivityOps.Permittivity

  object PermittivityConversions {
    import ops.permittivityOps.{ PermittivityConversions => Convs }

    lazy val faradPerMeter = Convs.faradPerMeter

    implicit class PermittivityConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.PermittivityConversions[A](a)

    implicit object PermittivityNumeric
        extends AbstractQuantityNumeric[PermittivityLike[Tuple], Tuple](
      Permittivity)
  }

  type Resistivity = ResistivityLike[Tuple]
  lazy val OhmMeters = ops.resistivityOps.OhmMeters
  lazy val Resistivity = ops.resistivityOps.Resistivity

  object ResistivityConversions {
    import ops.resistivityOps.{ ResistivityConversions => Convs }

    lazy val ohmMeter = Convs.ohmMeter

    implicit class ResistivityConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.ResistivityConversions[A](a)

    implicit object ResistivityNumeric
        extends AbstractQuantityNumeric[ResistivityLike[Tuple], Tuple](
      Resistivity)
  }

  lazy val ElementaryConstant =
    Coulombs(BigDecimal("1.602176565E-19").doubleValue)
}
