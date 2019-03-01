
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
  lazy val Slugs = ops.massOps.Slugs

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
    lazy val slug = Convs.slug

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

  type MolarMass = MolarMassLike[Tuple]
  lazy val KilogramsPerMole = ops.molarMassOps.KilogramsPerMole
  lazy val GramsPerMole = ops.molarMassOps.GramsPerMole

  object MolarMassConversions {
    import ops.molarMassOps.{ MolarMassConversions => Convs }

    lazy val kilogramPerMole = Convs.kilogramPerMole
    lazy val gramPerMole = Convs.gramPerMole

    implicit class MolarMassConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.MolarMassConversions[A](a)

    implicit object MolarMassNumeric
        extends AbstractQuantityNumeric[MolarMassLike[Tuple], Tuple](
      MolarMass)
  }

  val MolarMass = ops.molarMassOps.MolarMass

  type Molarity = MolarityLike[Tuple]
  lazy val MolesPerKilogram = ops.molarityOps.MolesPerKilogram

  object MolarityConversions {
    import ops.molarityOps.{ MolarityConversions => Convs }

    lazy val molePerKilogram = Convs.molePerKilogram

    implicit class MolarityConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.MolarityConversions[A](a)

    implicit object MolarityNumeric
        extends AbstractQuantityNumeric[MolarityLike[Tuple], Tuple](
      Molarity)
  }

  val Molarity = ops.molarityOps.Molarity

  type Concentration = ConcentrationLike[Tuple]
  lazy val MolesPerCubicMeter = ops.concentrationOps.MolesPerCubicMeter
  lazy val MolesPerLitre = ops.concentrationOps.MolesPerLitre
  lazy val MolesPerMillilitre = ops.concentrationOps.MolesPerMillilitre

  object ConcentrationConversions {
    import ops.concentrationOps.{ ConcentrationConversions => Convs }

    lazy val molePerCubicMeter = Convs.molePerCubicMeter
    lazy val molePerLitre = Convs.molePerLitre
    lazy val molePerMillilitre = Convs.molePerMillilitre

    implicit class ConcentrationConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.ConcentrationConversions[A](a)

    implicit object ConcentrationNumeric
        extends AbstractQuantityNumeric[ConcentrationLike[Tuple], Tuple](
      Concentration)
  }

  val Concentration = ops.concentrationOps.Concentration

  type CatalyticActivity = CatalyticActivityLike[Tuple]
  lazy val Katals = ops.catalyticActivityOps.Katals
  lazy val Decikatals = ops.catalyticActivityOps.Decikatals
  lazy val Centikatals = ops.catalyticActivityOps.Centikatals
  lazy val Millikatals = ops.catalyticActivityOps.Millikatals
  lazy val Microkatals = ops.catalyticActivityOps.Microkatals
  lazy val Nanokatals = ops.catalyticActivityOps.Nanokatals
  lazy val Decakatals = ops.catalyticActivityOps.Decakatals
  lazy val Hectokatals = ops.catalyticActivityOps.Hectokatals
  lazy val Kilokatals = ops.catalyticActivityOps.Kilokatals
  lazy val Megakatals = ops.catalyticActivityOps.Megakatals
  lazy val Gigakatals = ops.catalyticActivityOps.Gigakatals

  object CatalyticActivityConversions {
    import ops.catalyticActivityOps.{ CatalyticActivityConversions => Convs }

    lazy val katal = Convs.katal
    lazy val decikatal = Convs.decikatal
    lazy val centikatal = Convs.centikatal
    lazy val millikatal = Convs.millikatal
    lazy val microkatal = Convs.microkatal
    lazy val nanokatal = Convs.nanokatal
    lazy val decakatal = Convs.decakatal
    lazy val hectokatal = Convs.hectokatal
    lazy val kilokatal = Convs.kilokatal
    lazy val megakatal = Convs.megakatal
    lazy val gigakatal = Convs.gigakatal

    implicit class CatalyticActivityConversions[A](a: A)(
      implicit num: Numeric[A])
        extends Convs.CatalyticActivityConversions[A](a)

    implicit object CatalyticActivityNumeric
        extends AbstractQuantityNumeric[CatalyticActivityLike[Tuple], Tuple](
      CatalyticActivity)
  }

  val CatalyticActivity = ops.catalyticActivityOps.CatalyticActivity
}
