
package org.terra
package space

/**
  * Contains all the symbols exported by the org.terra.*.space packages.
  * Synthetic packages then subclass this trait with an object to represent
  * a package in the synthetic trees of symbols used by the unit test and
  * user's calling code.
  * @author Hunter Payne
  */
trait SpaceSymbols[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type Angle = AngleLike[Tuple]
  lazy val Radians = ops.angleOps.Radians
  lazy val Degrees = ops.angleOps.Degrees
  lazy val Gradians = ops.angleOps.Gradians
  lazy val Turns = ops.angleOps.Turns
  lazy val Arcminutes = ops.angleOps.Arcminutes
  lazy val Arcseconds = ops.angleOps.Arcseconds

  object AngleConversions {
    import ops.angleOps.{ AngleConversions => Convs }

    lazy val radian = Convs.radian
    lazy val degree = Convs.degree
    lazy val gradian = Convs.gradian
    lazy val turn = Convs.turn
    lazy val arcminute = Convs.arcminute
    lazy val arcsecond = Convs.arcsecond

    implicit class AngleConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.AngleConversions[A](a)

    implicit object AngleNumeric
        extends AbstractQuantityNumeric[AngleLike[Tuple], Tuple](Angle)
  }
  val Angle = ops.angleOps.Angle

  type Area = AreaLike[Tuple]
  lazy val SquareMeters = ops.areaOps.SquareMeters
  lazy val SquareCentimeters = ops.areaOps.SquareCentimeters
  lazy val SquareKilometers = ops.areaOps.SquareKilometers
  lazy val SquareUsMiles = ops.areaOps.SquareUsMiles
  lazy val SquareYards = ops.areaOps.SquareYards
  lazy val SquareFeet = ops.areaOps.SquareFeet
  lazy val SquareInches = ops.areaOps.SquareInches
  lazy val Hectares = ops.areaOps.Hectares
  lazy val Acres = ops.areaOps.Acres
  lazy val Barnes = ops.areaOps.Barnes

  object AreaConversions {
    import ops.areaOps.{ AreaConversions => Convs }

    lazy val squareMeter = Convs.squareMeter
    lazy val squareCentimeter = Convs.squareCentimeter
    lazy val squareKilometer = Convs.squareKilometer
    lazy val squareMile = Convs.squareMile
    lazy val squareYard = Convs.squareYard
    lazy val squareFoot = Convs.squareFoot
    lazy val squareInch = Convs.squareInch
    lazy val hectare = Convs.hectare
    lazy val acre = Convs.acre
    lazy val barn = Convs.barn

    implicit class AreaConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.AreaConversions[A](a)

    implicit object AreaNumeric
        extends AbstractQuantityNumeric[AreaLike[Tuple], Tuple](Area)
  }
  val Area = ops.areaOps.Area

  type Length = LengthLike[Tuple]
  lazy val Angstroms = ops.lengthOps.Angstroms
  lazy val Nanometers = ops.lengthOps.Nanometers
  lazy val Microns = ops.lengthOps.Microns
  lazy val Millimeters = ops.lengthOps.Millimeters
  lazy val Centimeters = ops.lengthOps.Centimeters
  lazy val Decimeters = ops.lengthOps.Decimeters
  lazy val Meters = ops.lengthOps.Meters
  lazy val Decameters = ops.lengthOps.Decameters
  lazy val Hectometers = ops.lengthOps.Hectometers
  lazy val Kilometers = ops.lengthOps.Kilometers
  lazy val Inches = ops.lengthOps.Inches
  lazy val Feet = ops.lengthOps.Feet
  lazy val Yards = ops.lengthOps.Yards
  lazy val UsMiles = ops.lengthOps.UsMiles
  lazy val InternationalMiles = ops.lengthOps.InternationalMiles
  lazy val NauticalMiles = ops.lengthOps.NauticalMiles
  lazy val AstronomicalUnits = ops.lengthOps.AstronomicalUnits
  lazy val LightYears = ops.lengthOps.LightYears
  lazy val Parsecs = ops.lengthOps.Parsecs
  lazy val KiloParsecs = ops.lengthOps.KiloParsecs
  lazy val MegaParsecs = ops.lengthOps.MegaParsecs
  lazy val GigaParsecs = ops.lengthOps.GigaParsecs
  lazy val SolarRadii = ops.lengthOps.SolarRadii
  lazy val NominalSolarRadii = ops.lengthOps.NominalSolarRadii
  lazy val ElectronVoltLength = ops.lengthOps.ElectronVoltLength
  lazy val MilliElectronVoltLength = ops.lengthOps.MilliElectronVoltLength
  lazy val KiloElectronVoltLength = ops.lengthOps.KiloElectronVoltLength
  lazy val MegaElectronVoltLength = ops.lengthOps.MegaElectronVoltLength
  lazy val GigaElectronVoltLength = ops.lengthOps.GigaElectronVoltLength
  lazy val TeraElectronVoltLength = ops.lengthOps.TeraElectronVoltLength
  lazy val PetaElectronVoltLength = ops.lengthOps.PetaElectronVoltLength
  lazy val ExaElectronVoltLength = ops.lengthOps.ExaElectronVoltLength

  object LengthConversions {
    import ops.lengthOps.{ LengthConversions => Convs }

    lazy val angstrom = Convs.angstrom
    lazy val nanometer = Convs.nanometer
    lazy val nanometre = Convs.nanometre
    lazy val micron = Convs.micron
    lazy val micrometer = Convs.micrometer
    lazy val micrometre = Convs.micrometre
    lazy val millimeter = Convs.millimeter
    lazy val millimetre = Convs.millimetre
    lazy val centimeter = Convs.centimeter
    lazy val centimetre = Convs.centimetre
    lazy val decimeter = Convs.decimeter
    lazy val decimetre = Convs.decimetre
    lazy val meter = Convs.meter
    lazy val metre = Convs.metre
    lazy val decameter = Convs.decameter
    lazy val decametre = Convs.decametre
    lazy val hectometer = Convs.hectometer
    lazy val hectometre = Convs.hectometre
    lazy val kilometer = Convs.kilometer
    lazy val kilometre = Convs.kilometre
    lazy val inch = Convs.inch
    lazy val foot = Convs.foot
    lazy val yard = Convs.yard
    lazy val mile = Convs.mile
    lazy val nauticalMile = Convs.nauticalMile
    lazy val astronomicalUnit = Convs.astronomicalUnit
    lazy val lightYear = Convs.lightYear
    lazy val parsec = Convs.parsec
    lazy val kiloparsec = Convs.kiloparsec
    lazy val megaparsec = Convs.megaparsec
    lazy val gigaparsec = Convs.gigaparsec
    lazy val solarRadius = Convs.solarRadius
    lazy val nominalSolarRadius = Convs.nominalSolarRadius

    lazy val eV = Convs.eV
    lazy val meV = Convs.meV
    lazy val keV = Convs.keV
    lazy val MeV = Convs.MeV
    lazy val GeV = Convs.GeV
    lazy val TeV = Convs.TeV
    lazy val PeV = Convs.PeV
    lazy val EeV = Convs.EeV

    implicit class LengthConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.LengthConversions[A](a)

    implicit class LengthStringConversions(s: String)
        extends Convs.LengthStringConversions(s)

    implicit object LengthNumeric
        extends AbstractQuantityNumeric[LengthLike[Tuple], Tuple](Length)
  }

  val Length = ops.lengthOps.Length

  type SolidAngle = SolidAngleLike[Tuple]
  lazy val SquareRadians = ops.solidAngleOps.SquareRadians

  object SolidAngleConversions {
    import ops.solidAngleOps.{ SolidAngleConversions => Convs }

    lazy val squareRadian = Convs.squareRadian
    lazy val steradian = Convs.steradian

    implicit class SolidAngleConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.SolidAngleConversions[A](a)

    implicit object SolidAngleNumeric
        extends AbstractQuantityNumeric[SolidAngleLike[Tuple], Tuple](
      SolidAngle)
  }
  val SolidAngle = ops.solidAngleOps.SolidAngle

  type Volume = VolumeLike[Tuple]
  lazy val CubicMeters = ops.volumeOps.CubicMeters
  lazy val Litres = ops.volumeOps.Litres
  lazy val Nanolitres = ops.volumeOps.Nanolitres
  lazy val Microlitres = ops.volumeOps.Microlitres
  lazy val Millilitres = ops.volumeOps.Millilitres
  lazy val Centilitres = ops.volumeOps.Centilitres
  lazy val Decilitres = ops.volumeOps.Decilitres
  lazy val Hectolitres = ops.volumeOps.Hectolitres
  lazy val CubicUsMiles = ops.volumeOps.CubicUsMiles
  lazy val CubicYards = ops.volumeOps.CubicYards
  lazy val CubicFeet = ops.volumeOps.CubicFeet
  lazy val CubicInches = ops.volumeOps.CubicInches
  lazy val UsGallons = ops.volumeOps.UsGallons
  lazy val UsQuarts = ops.volumeOps.UsQuarts
  lazy val UsPints = ops.volumeOps.UsPints
  lazy val UsCups = ops.volumeOps.UsCups
  lazy val FluidOunces = ops.volumeOps.FluidOunces
  lazy val Tablespoons = ops.volumeOps.Tablespoons
  lazy val Teaspoons = ops.volumeOps.Teaspoons
  lazy val AcreFeet = ops.volumeOps.AcreFeet

  object VolumeConversions {
    import ops.volumeOps.{ VolumeConversions => Convs }

    lazy val cubicMeter = Convs.cubicMeter
    lazy val litre = Convs.litre
    lazy val liter = Convs.liter
    lazy val nanolitre = Convs.nanolitre
    lazy val nanoliter = Convs.nanoliter
    lazy val microlitre = Convs.microlitre
    lazy val microliter = Convs.microliter
    lazy val millilitre = Convs.millilitre
    lazy val milliliter = Convs.milliliter
    lazy val centilitre = Convs.centilitre
    lazy val centiliter = Convs.centiliter
    lazy val decilitre = Convs.decilitre
    lazy val deciliter = Convs.deciliter
    lazy val hectolitre = Convs.hectolitre
    lazy val hectoliter = Convs.hectoliter

    lazy val cubicMile = Convs.cubicMile
    lazy val cubicYard = Convs.cubicYard
    lazy val cubicFoot = Convs.cubicFoot
    lazy val cubicInch = Convs.cubicInch

    lazy val gallon = Convs.gallon
    lazy val quart = Convs.quart
    lazy val pint = Convs.pint
    lazy val cup = Convs.cup

    lazy val fluidOunce = Convs.fluidOunce
    lazy val tablespoon = Convs.tablespoon
    lazy val teaspoon = Convs.teaspoon

    lazy val acreFoot = AcreFeet(1)

    implicit class VolumeConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.VolumeConversions[A](a)

    implicit object VolumeNumeric
        extends AbstractQuantityNumeric[VolumeLike[Tuple], Tuple](Volume)
  }
  val Volume = ops.volumeOps.Volume

  type SpecificVolume = SpecificVolumeLike[Tuple]
  lazy val CubicMetersPerKilogram = ops.specificVolumeOps.CubicMetersPerKilogram
  lazy val MillilitresPerGram = ops.specificVolumeOps.MillilitresPerGram
  lazy val CubicFeetPerPound = ops.specificVolumeOps.CubicFeetPerPound
  lazy val CubicFeetPerSlug = ops.specificVolumeOps.CubicFeetPerSlug

  object SpecificVolumeConversions {
    import ops.specificVolumeOps.{ SpecificVolumeConversions => Convs }

    lazy val cubicMeterPerKilogram = Convs.cubicMeterPerKilogram
    lazy val millilitrePerGram = Convs.millilitrePerGram
    lazy val cubicFootPerPound = Convs.cubicFootPerPound
    lazy val cubicFootPerSlug = Convs.cubicFootPerSlug

    implicit class SpecificVolumeConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.SpecificVolumeConversions[A](a)

    implicit object SpecificVolumeNumeric
        extends AbstractQuantityNumeric[SpecificVolumeLike[Tuple], Tuple](
      SpecificVolume)
  }
  val SpecificVolume = ops.specificVolumeOps.SpecificVolume

  type MolarVolume = MolarVolumeLike[Tuple]
  lazy val CubicMetersPerMole = ops.molarVolumeOps.CubicMetersPerMole

  object MolarVolumeConversions {
    import ops.molarVolumeOps.{ MolarVolumeConversions => Convs }

    lazy val cubicMeterPerMole = Convs.cubicMeterPerMole
    implicit class MolarVolumeConversions[A](a: A)(implicit n: Numeric[A])
        extends Convs.MolarVolumeConversions[A](a)

    implicit object MolarVolumeNumeric
        extends AbstractQuantityNumeric[MolarVolumeLike[Tuple], Tuple](
      MolarVolume)
  }
  val MolarVolume = ops.molarVolumeOps.MolarVolume
}
