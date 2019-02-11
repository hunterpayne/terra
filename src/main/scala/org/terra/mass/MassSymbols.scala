
package org.terra
package mass

/**
  * Contains all the symbols exported by the org.terra.*.mass packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * @author Hunter Payne
  */
trait MassSymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type AreaDensity = AreaDensityLike[Tuple]
  lazy val KilogramsPerSquareMeter =
    ops.areaDensityOps.KilogramsPerSquareMeter
  lazy val KilogramsPerHectare = ops.areaDensityOps.KilogramsPerHectare
  lazy val GramsPerSquareCentimeter =
    ops.areaDensityOps.GramsPerSquareCentimeter
  lazy val PoundsPerAcre = ops.areaDensityOps.PoundsPerAcre

  object AreaDensityConversions {
    import ops.areaDensityOps.{ AreaDensityConversions => Convs }

    lazy val kilogramPerSquareMeter = Convs.kilogramPerSquareMeter
    lazy val kilogramPerHectare = Convs.kilogramPerHectare
    lazy val gramPerSquareCentimeter = Convs.gramPerSquareCentimeter
    lazy val poundsPerAcre = Convs.poundsPerAcre

    implicit class AreaDensityConversions[A](a: A)(implicit num: Numeric[A])
        extends Convs.AreaDensityConversions[A](a)

    implicit object AreaDensityNumeric
        extends AbstractQuantityNumeric[AreaDensityLike[Tuple], Tuple](
      AreaDensity)
  }

  val AreaDensity = ops.areaDensityOps.AreaDensity

  type ChemicalAmount = ChemicalAmountLike[Tuple]
  lazy val Moles = ops.chemicalAmountOps.Moles
  lazy val PoundMoles = ops.chemicalAmountOps.PoundMoles

  object ChemicalAmountConversions {
    import ops.chemicalAmountOps.{ ChemicalAmountConversions => Convs }

    lazy val mole = Convs.mole
    lazy val poundMole = Convs.poundMole

    implicit class ChemicalAmountConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.ChemicalAmountConversions[A](a)

    implicit object ChemicalAmountNumeric
        extends AbstractQuantityNumeric[ChemicalAmountLike[Tuple], Tuple](
      ChemicalAmount)
  }

  val ChemicalAmount = ops.chemicalAmountOps.ChemicalAmount

  type Density = DensityLike[Tuple]
  lazy val KilogramsPerCubicMeter = ops.densityOps.KilogramsPerCubicMeter

  object DensityConversions {
    import ops.densityOps.{ DensityConversions => Convs }

    lazy val kilogramPerCubicMeter = Convs.kilogramPerCubicMeter

    implicit class DensityConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.DensityConversions[A](a)

    implicit object DensityNumeric
        extends AbstractQuantityNumeric[DensityLike[Tuple], Tuple](Density)
  }

  val Density = ops.densityOps.Density

  type Mass = MassLike[Tuple]
  lazy val Nanograms = ops.massOps.Nanograms
  lazy val Micrograms = ops.massOps.Micrograms
  lazy val Milligrams = ops.massOps.Milligrams
  lazy val Grams = ops.massOps.Grams
  lazy val Kilograms = ops.massOps.Kilograms
  lazy val Tonnes = ops.massOps.Tonnes
  lazy val Ounces = ops.massOps.Ounces
  lazy val Pounds = ops.massOps.Pounds
  lazy val Kilopounds = ops.massOps.Kilopounds
  lazy val Megapounds = ops.massOps.Megapounds
  lazy val Stone = ops.massOps.Stone
  lazy val TroyGrains = ops.massOps.TroyGrains
  lazy val Pennyweights = ops.massOps.Pennyweights
  lazy val TroyOunces = ops.massOps.TroyOunces
  lazy val TroyPounds = ops.massOps.TroyPounds
  lazy val Tolas = ops.massOps.Tolas
  lazy val Carats = ops.massOps.Carats
  lazy val SolarMasses = ops.massOps.SolarMasses
  lazy val ElectronVoltMass = ops.massOps.ElectronVoltMass
  lazy val MilliElectronVoltMass = ops.massOps.MilliElectronVoltMass
  lazy val KiloElectronVoltMass = ops.massOps.KiloElectronVoltMass
  lazy val MegaElectronVoltMass = ops.massOps.MegaElectronVoltMass
  lazy val GigaElectronVoltMass = ops.massOps.GigaElectronVoltMass
  lazy val TeraElectronVoltMass = ops.massOps.TeraElectronVoltMass
  lazy val PetaElectronVoltMass = ops.massOps.PetaElectronVoltMass
  lazy val ExaElectronVoltMass = ops.massOps.ExaElectronVoltMass

  object MassConversions {
    import ops.massOps.{ MassConversions => Convs }

    lazy val nanogram = Convs.nanogram
    lazy val microgram = Convs.microgram
    lazy val milligram = Convs.milligram
    lazy val gram = Convs.gram
    lazy val kilogram = Convs.kilogram
    lazy val tonne = Convs.tonne
    lazy val ounce = Convs.ounce
    lazy val pound = Convs.pound
    lazy val kilopound = Convs.kilopound
    lazy val megapound = Convs.megapound
    lazy val stone = Convs.stone
    lazy val troyGrain = Convs.troyGrain
    lazy val pennyweight = Convs.pennyweight
    lazy val troyOunce = Convs.troyOunce
    lazy val troyPound = Convs.troyPound
    lazy val tola = Convs.tola
    lazy val carat = Convs.carat
    lazy val solarMass = Convs.solarMass

    lazy val eV = Convs.eV
    lazy val meV = Convs.meV
    lazy val keV = Convs.keV
    lazy val MeV = Convs.MeV
    lazy val GeV = Convs.GeV
    lazy val TeV = Convs.TeV
    lazy val PeV = Convs.PeV
    lazy val EeV = Convs.EeV

    implicit class MassConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.MassConversions[A](a)

    implicit class MassStringConversions(s: String)
        extends Convs.MassStringConversions(s)

    implicit object MassNumeric
        extends AbstractQuantityNumeric[MassLike[Tuple], Tuple](Mass)
  }

  val Mass = ops.massOps.Mass

  type MomentOfInertia = MomentOfInertiaLike[Tuple]
  lazy val KilogramsMetersSquared =
    ops.momentOfInertiaOps.KilogramsMetersSquared
  lazy val PoundsSquareFeet = ops.momentOfInertiaOps.PoundsSquareFeet

  object MomentOfInertiaConversions {
    import ops.momentOfInertiaOps.{ MomentOfInertiaConversions => Convs }

    lazy val kilogramMetersSquared = Convs.kilogramMetersSquared
    lazy val poundSquareFeet = Convs.poundSquareFeet

    implicit class MomentOfInertiaConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.MomentOfInertiaConversions[A](a)

    implicit object MomentOfInertiaNumeric
        extends AbstractQuantityNumeric[MomentOfInertiaLike[Tuple], Tuple](
      MomentOfInertia)
  }

  val MomentOfInertia = ops.momentOfInertiaOps.MomentOfInertia
}
