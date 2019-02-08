/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package space

import scala.reflect.ClassTag

import org.terra.electro._
import org.terra.energy.{ PowerLike, EnergyLike }
import org.terra.motion.{ VelocityLike, AccelerationLike, ForceLike }
import org.terra.radio.{ RadiantIntensityLike, SpectralIntensityLike, SpectralPowerLike }
import org.terra.time.{ SecondTimeIntegral, TimeIntegral, TimeSquaredLike, TimeLike }

/**
 * Represents a quantity of length
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in  [[org.terra.space.Meters]]
 */
final class LengthLike[C <: TypeContext](val value: C#T, val unit: LengthUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[LengthLike[C], C#T, C]
    with TimeIntegral[VelocityLike[C], C#T, C#T, C]
    with SecondTimeIntegral[AccelerationLike[C], C] {

  import ops.lengthOps._
  import ops.areaOps.{ SquareCentimeters, SquareKilometers, SquareUsMiles, SquareYards, SquareFeet, SquareInches, SquareMeters }
  import ops.volumeOps.{ CubicYards, CubicFeet, CubicInches, CubicMeters }
  import ops.energyOps.Joules
  import ops.radiantIntensityOps.WattsPerSteradian
  import ops.powerOps.Watts
  import ops.electricalConductanceOps.Siemens
  import ops.resistivityOps.OhmMeters
  import ops.timeOps.Seconds
  import ops.velocityOps.MetersPerSecond

  type Area = AreaLike[C]
  type Volume = VolumeLike[C]
  type Force = ForceLike[C]
  type Energy = EnergyLike[C]
  type SpectralIntensity = SpectralIntensityLike[C]
  type RadiantIntensity = RadiantIntensityLike[C]
  type SpectralPower = SpectralPowerLike[C]
  type Power = PowerLike[C]
  type Conductivity = ConductivityLike[C]
  type ElectricalConductance = ElectricalConductanceLike[C]
  type ElectricalResistance = ElectricalResistanceLike[C]
  type Resistivity = ResistivityLike[C]
  type TimeSquared = TimeSquaredLike[C]
  type Acceleration = AccelerationLike[C]
  type Time = TimeLike[C]
  type Length = LengthLike[C]

  def dimension: Dimension[LengthLike[C], C#T, C] = Length

  protected def timeDerived = MetersPerSecond(toMeters)
  protected[terra] def time = Seconds(1)

  def *(that: Length)(implicit ops: TerraOps[C]): Area = unit match {
    case Centimeters ⇒ SquareCentimeters(ops.num.times(this.value, that.toCentimeters))
    case Kilometers  ⇒ SquareKilometers(ops.num.times(this.value, that.toKilometers))
    case UsMiles     ⇒ SquareUsMiles(ops.num.times(this.value, that.toUsMiles))
    case Yards       ⇒ SquareYards(ops.num.times(this.value, that.toYards))
    case Feet        ⇒ SquareFeet(ops.num.times(this.value, that.toFeet))
    case Inches      ⇒ SquareInches(ops.num.times(this.value, that.toInches))
    case _           ⇒ SquareMeters(ops.num.times(toMeters, that.toMeters))
  }

  def *(that: Area)(implicit ops: TerraOps[C]): Volume = unit match {
    case Yards  ⇒ CubicYards(ops.num.times(this.value, that.toSquareYards))
    case Feet   ⇒ CubicFeet(ops.num.times(this.value, that.toSquareFeet))
    case Inches ⇒ CubicInches(ops.num.times(this.value, that.toSquareInches))
    case _      ⇒ CubicMeters(ops.num.times(this.toMeters, that.toSquareMeters))
  }

  def *(that: Force)(implicit ops: TerraOps[C]): Energy = 
    Joules(ops.num.times(this.toMeters, that.toNewtons))
  def *(that: SpectralIntensity)(implicit ops: TerraOps[C]): RadiantIntensity = 
    WattsPerSteradian(ops.num.times(this.toMeters, that.toWattsPerSteradianPerMeter))
  def *(that: SpectralPower)(implicit ops: TerraOps[C]): Power = 
    Watts(ops.num.times(this.toMeters, that.toWattsPerMeter))
  def *(that: Conductivity)(implicit ops: TerraOps[C]): ElectricalConductance = 
    Siemens(ops.num.times(this.toMeters, that.toSiemensPerMeter))
  def *(that: ElectricalResistance)(implicit ops: TerraOps[C]): Resistivity =
    OhmMeters(ops.num.times(this.toMeters, that.toOhms))

  def /(that: TimeSquared)(implicit ops: TerraOps[C]): Acceleration = 
    this / that.time1 / that.time2
  def /(that: Acceleration)(implicit ops: TerraOps[C]): TimeSquared =
    (this / that.timeIntegrated) * time

  def squared = this * this
  def cubed = this * this * this

  def toAngstroms = to(Angstroms)
  def toNanometers = to(Nanometers)
  def toMicrons = to(Microns)
  def toMillimeters = to(Millimeters)
  def toCentimeters = to(Centimeters)
  def toDecimeters = to(Decimeters)
  def toMeters = to(Meters)
  def toDecameters = to(Decameters)
  def toHectometers = to(Hectometers)
  def toKilometers = to(Kilometers)
  def toInches = to(Inches)
  def toFeet = to(Feet)
  def toYards = to(Yards)
  def toUsMiles = to(UsMiles)
  def toInternationalMiles = to(InternationalMiles)
  def toNauticalMiles = to(NauticalMiles)
  def toAstronomicalUnits = to(AstronomicalUnits)
  def toLightYears = to(LightYears)
  def toParsecs = to(Parsecs)
  def toKiloParsecs = to(KiloParsecs)
  def toMegaParsecs = to(MegaParsecs)
  def toGigaParsecs = to(GigaParsecs)
  def toSolarRadii = to(SolarRadii)
  def toNominalSolarRadii = to(NominalSolarRadii)
  def toeV = to(ElectronVoltLength)
  def tomeV = to(MilliElectronVoltLength)
  def tokeV = to(KiloElectronVoltLength)
  def toMeV = to(MegaElectronVoltLength)
  def toGeV = to(GigaElectronVoltLength)
  def toTeV = to(TeraElectronVoltLength)
  def toPeV = to(PetaElectronVoltLength)
  def toEeV = to(ExaElectronVoltLength)

}

/**
 * Base trait for units of [[org.terra.space.Length]]
 */
trait LengthUnit[C <: TypeContext] extends UnitOfMeasure[LengthLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new LengthLike[C](t, this)
}

trait LengthOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait LengthUnitT extends LengthUnit[C]

  /**
    * Factory singleton for length
    */
  object Length extends Dimension[LengthLike[C], C#T, C] with BaseDimension {
    private[space] def apply[A](a: A, unit: LengthUnit[C])(
      implicit n: Numeric[A]) = 
      new LengthLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Length"
    def primaryUnit = Meters
    def siUnit = Meters
    def units = Set(Angstroms, Nanometers, Microns, Millimeters, Centimeters,
      Decimeters, Meters, Decameters, Hectometers, Kilometers,
      Inches, Feet, Yards, UsMiles, InternationalMiles, NauticalMiles,
      AstronomicalUnits, LightYears, Parsecs, KiloParsecs, MegaParsecs, GigaParsecs, SolarRadii, NominalSolarRadii,
      ElectronVoltLength, MilliElectronVoltLength, KiloElectronVoltLength, MegaElectronVoltLength,
      GigaElectronVoltLength, TeraElectronVoltLength, PetaElectronVoltLength, ExaElectronVoltLength)
    def dimensionSymbol = "L"
  }

  object Angstroms extends LengthUnitT {
    // note: the symbol used here is the letter "\u00C5" which is to be preferred over the angstrom sign "\u212B"
    // see also: https://en.wikipedia.org/wiki/Å and http://www.fileformat.info/info/unicode/char/00c5/index.htm
    val symbol = "Å"
    val conversionFactor = 100 * MetricSystem.Pico
  }

  object Nanometers extends LengthUnitT with SiUnit {
    val symbol = "nm"
    val conversionFactor = MetricSystem.Nano
  }

  object Microns extends LengthUnitT with SiUnit {
    val symbol = "µm"
    val conversionFactor = MetricSystem.Micro
  }

  object Millimeters extends LengthUnitT with SiUnit {
    val symbol = "mm"
    val conversionFactor = MetricSystem.Milli
  }

  object Centimeters extends LengthUnitT with SiUnit {
    val symbol = "cm"
    val conversionFactor = MetricSystem.Centi
  }

  object Decimeters extends LengthUnitT with SiUnit {
    val symbol = "dm"
    val conversionFactor = MetricSystem.Deci
  }

  object Meters extends LengthUnitT with PrimaryUnit[C#T, C] with SiBaseUnit {
    val symbol = "m"
  }

  object Decameters extends LengthUnitT with SiUnit {
    val symbol = "dam"
    val conversionFactor = MetricSystem.Deca
  }

  object Hectometers extends LengthUnitT with SiUnit {
    val symbol = "hm"
    val conversionFactor = MetricSystem.Hecto
  }

  object Kilometers extends LengthUnitT with SiUnit {
    val symbol = "km"
    val conversionFactor = MetricSystem.Kilo
  }

  object Inches extends LengthUnitT {
    val conversionFactor = Feet.conversionFactor / 12d
    val symbol = "in"
  }

  object Feet extends LengthUnitT {
    val conversionFactor = 3.048006096e-1
    val symbol = "ft"
  }

  object Yards extends LengthUnitT {
    val conversionFactor = Feet.conversionFactor * 3d
    val symbol = "yd"
  }

  object UsMiles extends LengthUnitT {
    val conversionFactor = Feet.conversionFactor * 5.28e3
    val symbol = "mi"
  }

  object InternationalMiles extends LengthUnitT {
    val conversionFactor = 1.609344e3
    val symbol = "mile"
  }

  object NauticalMiles extends LengthUnitT {
    val conversionFactor = 1.852e3
    val symbol = "nmi"
  }

  object AstronomicalUnits extends LengthUnitT {
    val conversionFactor = 1.495978707e11
    val symbol = "au"
  }

  object LightYears extends LengthUnitT {
    val conversionFactor = 9.4607304725808e15
    val symbol = "ly"
  }

  object Parsecs extends LengthUnitT {
    val conversionFactor = 3.08567758149137e16
    val symbol = "pc"
  }

  object KiloParsecs extends LengthUnitT {
    val conversionFactor = Parsecs.conversionFactor * MetricSystem.Kilo
    val symbol = "kpc"
  }

  object MegaParsecs extends LengthUnitT {
    val conversionFactor = Parsecs.conversionFactor * MetricSystem.Mega
    val symbol = "Mpc"
  }

  object GigaParsecs extends LengthUnitT {
    val conversionFactor = Parsecs.conversionFactor * MetricSystem.Giga
    val symbol = "Gpc"
  }

  object SolarRadii extends LengthUnitT {
    val conversionFactor = 6.957e8
    val symbol = "R☉"
  }

  object NominalSolarRadii extends LengthUnitT {
    val conversionFactor = 6.957e8
    val symbol = "RN☉"
  }

  object ElectronVoltLength extends LengthUnitT {
    val conversionFactor = 1.97327e-7
    val symbol = "ħc/eV"
  }

  object MilliElectronVoltLength extends LengthUnitT {
    val conversionFactor = ElectronVoltLength.conversionFactor * MetricSystem.Milli
    val symbol = "mħc/eV"
  }

  object KiloElectronVoltLength extends LengthUnitT {
    val conversionFactor = ElectronVoltLength.conversionFactor * MetricSystem.Kilo
    val symbol = "kħc/eV"
  }

  object MegaElectronVoltLength extends LengthUnitT {
    val conversionFactor = ElectronVoltLength.conversionFactor * MetricSystem.Mega
    val symbol = "Mħc/eV"
  }

  object GigaElectronVoltLength extends LengthUnitT {
    val conversionFactor = ElectronVoltLength.conversionFactor * MetricSystem.Giga
    val symbol = "Għc/eV"
  }

  object TeraElectronVoltLength extends LengthUnitT {
    val conversionFactor = ElectronVoltLength.conversionFactor * MetricSystem.Tera
    val symbol = "Tħc/eV"
  }

  object PetaElectronVoltLength extends LengthUnitT {
    val conversionFactor = ElectronVoltLength.conversionFactor * MetricSystem.Peta
    val symbol = "Pħc/eV"
  }

  object ExaElectronVoltLength extends LengthUnitT {
    val conversionFactor = ElectronVoltLength.conversionFactor * MetricSystem.Exa
    val symbol = "Eħc/eV"
  }

  object LengthConversions {
    lazy val angstrom = Angstroms(1)
    lazy val nanometer = Nanometers(1)
    lazy val nanometre = Nanometers(1)
    lazy val micron = Microns(1)
    lazy val micrometer = Microns(1)
    lazy val micrometre = Microns(1)
    lazy val millimeter = Millimeters(1)
    lazy val millimetre = Millimeters(1)
    lazy val centimeter = Centimeters(1)
    lazy val centimetre = Centimeters(1)
    lazy val decimeter = Decimeters(1)
    lazy val decimetre = Decimeters(1)
    lazy val meter = Meters(1)
    lazy val metre = Meters(1)
    lazy val decameter = Decameters(1)
    lazy val decametre = Decameters(1)
    lazy val hectometer = Hectometers(1)
    lazy val hectometre = Hectometers(1)
    lazy val kilometer = Kilometers(1)
    lazy val kilometre = Kilometers(1)
    lazy val inch = Inches(1)
    lazy val foot = Feet(1)
    lazy val yard = Yards(1)
    lazy val mile = UsMiles(1)
    lazy val nauticalMile = NauticalMiles(1)
    lazy val astronomicalUnit = AstronomicalUnits(1)
    lazy val lightYear = LightYears(1)
    lazy val parsec = Parsecs(1)
    lazy val kiloparsec = KiloParsecs(1)
    lazy val megaparsec = MegaParsecs(1)
    lazy val gigaparsec = GigaParsecs(1)
    lazy val solarRadius = SolarRadii(1)
    lazy val nominalSolarRadius = NominalSolarRadii(1)

    lazy val eV = ElectronVoltLength(1)
    lazy val meV = MilliElectronVoltLength(1)
    lazy val keV = KiloElectronVoltLength(1)
    lazy val MeV = MegaElectronVoltLength(1)
    lazy val GeV = GigaElectronVoltLength(1)
    lazy val TeV = TeraElectronVoltLength(1)
    lazy val PeV = PetaElectronVoltLength(1)
    lazy val EeV = ExaElectronVoltLength(1)

    implicit class LengthConversions[A](a: A)(implicit n: Numeric[A]) {
      def Å = Angstroms(a)
      def angstroms = Angstroms(a)
      def nm = Nanometers(a)
      def nanometers = Nanometers(a)
      def nanometres = Nanometers(a)
      def µm = Microns(a)
      def microns = Microns(a)
      def micrometer = Microns(a)
      def micrometre = Microns(a)
      def mm = Millimeters(a)
      def millimeters = Millimeters(a)
      def millimetres = Millimeters(a)
      def cm = Centimeters(a)
      def centimeters = Centimeters(a)
      def centimetres = Centimeters(a)
      def dm = Decimeters(a)
      def meters = Meters(a)
      def metres = Meters(a)
      def dam = Decameters(a)
      def hm = Hectometers(a)
      def km = Kilometers(a)
      def kilometers = Kilometers(a)
      def kilometres = Kilometers(a)
      def inches = Inches(a)
      def ft = Feet(a)
      def feet = Feet(a)
      def yd = Yards(a)
      def yards = Yards(a)
      def miles = UsMiles(a)
      def nmi = NauticalMiles(a)
      def au = AstronomicalUnits(a)
      def ly = LightYears(a)
      def lightYears = LightYears(a)
      def parsecs = Parsecs(a)
      def pc = Parsecs(a)
      def kpc = KiloParsecs(a)
      def Mpc = MegaParsecs(a)
      def Gpc = GigaParsecs(a)
      def solarRadii = SolarRadii(a)
      def nominalSolarRadii = NominalSolarRadii(a)
      def eV = ElectronVoltLength(a)
      def meV = MilliElectronVoltLength(a)
      def keV = KiloElectronVoltLength(a)
      def MeV = MegaElectronVoltLength(a)
      def GeV = GigaElectronVoltLength(a)
      def TeV = TeraElectronVoltLength(a)
      def PeV = PetaElectronVoltLength(a)
      def EeV = ExaElectronVoltLength(a)
    }

    implicit class LengthStringConversions(s: String) {
      def toLength = Length(s)
    }
  }
}


