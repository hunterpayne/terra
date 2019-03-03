/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package mass

import time.{ TimeIntegral, TimeLike }
import energy.{ EnergyLike, SpecificEnergyLike }
import space.{ LengthLike, AreaLike, VolumeLike }
import motion.{ AccelerationLike, ForceLike, MassFlowLike, VelocityLike, MomentumLike }

/**
 * Represents a quantity of Mass
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value the value in the [[org.terra.standard.mass.Grams]]
 */
final class MassLike[C <: TypeContext](val value: C#T, val unit: MassUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[MassLike[C], C#T, C]
    with TimeIntegral[MassFlowLike[C], C#T, C#T, C] {

  import ops.massOps._
  import ops.massFlowOps.KilogramsPerSecond
  import ops.timeOps.Seconds
  import ops.energyOps.Joules
  import ops.forceOps.Newtons
  import ops.massFlowOps.KilogramsPerSecond
  import ops.momentumOps.Momentum
  import ops.volumeOps.CubicMeters
  import ops.densityOps.Density
  import ops.areaOps.SquareMeters
  import ops.areaDensityOps.KilogramsPerSquareMeter
  import ops.momentOfInertiaOps.KilogramsMetersSquared
  import ops.molarMassOps.KilogramsPerMole
  import ops.chemicalAmountOps.Moles

  type Acceleration = AccelerationLike[C]
  type Energy = EnergyLike[C]
  type Momentum = MomentumLike[C]
  type Velocity = VelocityLike[C]
  type Force = ForceLike[C]
  type Density = DensityLike[C]
  type AreaDensity = AreaDensityLike[C]
  type Area = AreaLike[C]
  type Volume = VolumeLike[C]
  type Length = LengthLike[C]
  type SpecificEnergy = SpecificEnergyLike[C]
  type MomentOfInertia = MomentOfInertiaLike[C]
  type ChemicalAmount = ChemicalAmountLike[C]
  type MolarMass = MolarMassLike[C]

  def dimension: Dimension[MassLike[C], C#T, C] = Mass

  protected def timeDerived = KilogramsPerSecond(toKilograms)
  protected def time = Seconds(1)

  def *(that: SpecificEnergy)(implicit ops: TerraOps[C]): Energy = {
    Joules(ops.num.times(this.toKilograms, that.toJoulesPerKilogram))
  }
  def *(that: Velocity): Momentum = Momentum(this, that)
  def *(that: Acceleration)(implicit ops: TerraOps[C]): Force = {
    Newtons(ops.num.times(this.toKilograms, that.toMetersPerSecondSquared))
  }
  def /(that: Density)(implicit ops: TerraOps[C]): Volume = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CubicMeters(ops.div[C#T](this.toKilograms, that.toKilogramsPerCubicMeter))
  }
  def /(that: Volume)(implicit ops: TerraOps[C]): Density = 
    Density(this, that)
  def /(that: AreaDensity)(implicit ops: TerraOps[C]): Area = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    SquareMeters(ops.div[C#T](this.toKilograms, that.toKilogramsPerSquareMeter))
  }
  def /(that: Area)(implicit ops: TerraOps[C]): AreaDensity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    KilogramsPerSquareMeter(ops.div[C#T](this.toKilograms, that.toSquareMeters))
  }
  def /(that: ChemicalAmount)(implicit ops: TerraOps[C]): MolarMass = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    KilogramsPerMole(ops.div[C#T](this.toKilograms, that.toMoles))
  }
  def /(that: MolarMass)(implicit ops: TerraOps[C]): ChemicalAmount = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Moles(ops.div[C#T](this.toKilograms, that.toKilogramsPerMole))
  }

  /**
    * Moment of inertia of a point mass with with this mass and the given
    * radius from the center of rotation
    * @param radius length to center of rotation
    * @return moment of inertia of a point mass with given mass and radius
    */
  def onRadius(radius: Length)(implicit ops: TerraOps[C]): MomentOfInertia = 
    KilogramsMetersSquared(
      ops.num.times(toKilograms, radius.squared.toSquareMeters))

  def toNanograms = to(Nanograms)
  def toMicrograms = to(Micrograms)
  def toMilligrams = to(Milligrams)
  def toGrams = to(Grams)
  def toKilograms = to(Kilograms)
  def toTonnes = to(Tonnes)
  def toOunces = to(Ounces)
  def toPounds = to(Pounds)
  def toKilopounds = to(Kilopounds)
  def toMegapounds = to(Megapounds)
  def toStone = to(Stone)
  def toTroyGrains = to(TroyGrains)
  def toPennyweights = to(Pennyweights)
  def toTroyOunces = to(TroyOunces)
  def toTroyPounds = to(TroyPounds)
  def toTolas = to(Tolas)
  def toCarats = to(Carats)
  def toSolarMasses = to(SolarMasses)

  def toeV = to(ElectronVoltMass)
  def tomeV = to(MilliElectronVoltMass)
  def tokeV = to(KiloElectronVoltMass)
  def toMeV = to(MegaElectronVoltMass)
  def toGeV = to(GigaElectronVoltMass)
  def toTeV = to(TeraElectronVoltMass)
  def toPeV = to(PetaElectronVoltMass)
  def toEeV = to(ExaElectronVoltMass)
}

/**
 * Base trait for units of [[org.terra.mass.MassLike]]
 */
trait MassUnit[C <: TypeContext] extends UnitOfMeasure[MassLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new MassLike[C](t, this)
}

trait MassOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]
  //def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait MassUnitT extends MassUnit[C]

  /**
    * Factory singleton for [[org.terra.mass.MassLike]] values
    */
  object Mass extends Dimension[MassLike[C], C#T, C] with BaseDimension {
    private[mass] def apply[A](a: A, unit: MassUnit[C])(
      implicit n: Numeric[A], ops: TerraOps[C]) = 
      new MassLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Mass"
    def primaryUnit = Grams
    def siUnit = Kilograms
    def units = Set(Nanograms, Micrograms, Milligrams, Grams, Kilograms, Tonnes, Ounces, Pounds, Kilopounds, Megapounds,
      Stone, TroyGrains, Pennyweights, TroyOunces, TroyPounds, Tolas, Carats, SolarMasses,
      ElectronVoltMass, MilliElectronVoltMass, KiloElectronVoltMass, MegaElectronVoltMass,
      GigaElectronVoltMass, TeraElectronVoltMass, PetaElectronVoltMass, ExaElectronVoltMass, Slugs)
    def dimensionSymbol = "M"
  }

  object Grams extends MassUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "g"
  }

  object Nanograms extends MassUnitT with SiUnit {
    val conversionFactor = MetricSystem.Nano
    val symbol = "ng"
  }

  object Micrograms extends MassUnitT with SiUnit {
    val conversionFactor = MetricSystem.Micro
    val symbol = "mcg"
  }

  object Milligrams extends MassUnitT with SiUnit {
    val conversionFactor = MetricSystem.Milli
    val symbol = "mg"
  }

  object Kilograms extends MassUnitT with SiBaseUnit {
    val conversionFactor = MetricSystem.Kilo
    val symbol = "kg"
  }

  object Tonnes extends MassUnitT {
    val conversionFactor = MetricSystem.Mega
    val symbol = "t"
  }

  object Ounces extends MassUnitT {
    val conversionFactor = Pounds.conversionFactor / 16d
    val symbol = "oz"
  }

  object Pounds extends MassUnitT {
    val conversionFactor = Kilograms.conversionFactor * 4.5359237e-1
    val symbol = "lb"
  }

  object Kilopounds extends MassUnitT {
    val conversionFactor = Pounds.conversionFactor * MetricSystem.Kilo
    val symbol = "klb"
  }

  object Megapounds extends MassUnitT {
    val conversionFactor = Pounds.conversionFactor * MetricSystem.Kilo
    val symbol = "Mlb"
  }

  object Stone extends MassUnitT {
    val conversionFactor = Pounds.conversionFactor * 14d
    val symbol = "st"
  }

  object TroyGrains extends MassUnitT {
    val conversionFactor = 64.79891 * Milligrams.conversionFactor
    val symbol = "gr"
  }

  object Pennyweights extends MassUnitT {
    val conversionFactor = 24d * TroyGrains.conversionFactor
    val symbol = "dwt"
  }

  object TroyOunces extends MassUnitT {
    val conversionFactor = 480d * TroyGrains.conversionFactor
    val symbol = "oz t"
  }

  object TroyPounds extends MassUnitT {
    val conversionFactor = 12d * TroyOunces.conversionFactor
    val symbol = "lb t"
  }

  object Tolas extends MassUnitT {
    val conversionFactor = 180d * TroyGrains.conversionFactor
    val symbol = "tola"
  }

  object Carats extends MassUnitT {
    val conversionFactor = 200d * Milligrams.conversionFactor
    val symbol = "ct"
  }

  object SolarMasses extends MassUnitT {
    val conversionFactor = 1.98855e33
    val symbol = "M☉"
  }

  object ElectronVoltMass extends MassUnitT {
    val conversionFactor = 1.782662e-36
    val symbol = "eV/c²"
  }

  object MilliElectronVoltMass extends MassUnitT {
    val conversionFactor = ElectronVoltMass.conversionFactor * MetricSystem.Milli
    val symbol = "meV/c²"
  }

  object KiloElectronVoltMass extends MassUnitT {
    val conversionFactor = ElectronVoltMass.conversionFactor * MetricSystem.Kilo
    val symbol = "keV/c²"
  }

  object MegaElectronVoltMass extends MassUnitT {
    val conversionFactor = ElectronVoltMass.conversionFactor * MetricSystem.Mega
    val symbol = "MeV/c²"
  }

  object GigaElectronVoltMass extends MassUnitT {
    val conversionFactor = ElectronVoltMass.conversionFactor * MetricSystem.Giga
    val symbol = "GeV/c²"
  }

  object TeraElectronVoltMass extends MassUnitT {
    val conversionFactor = ElectronVoltMass.conversionFactor * MetricSystem.Tera
    val symbol = "TeV/c²"
  }

  object PetaElectronVoltMass extends MassUnitT {
    val conversionFactor = ElectronVoltMass.conversionFactor * MetricSystem.Peta
    val symbol = "PeV/c²"
  }

  object ExaElectronVoltMass extends MassUnitT {
    val conversionFactor = ElectronVoltMass.conversionFactor * MetricSystem.Exa
    val symbol = "EeV/c²"
  }

  object Slugs extends MassUnitT {
    val conversionFactor = 14.59390
    val symbol = "slug"
  }

  /**
    * Implicit conversions for [[org.terra.mass.MassLike]]
    *
    * Provides support fot the DSL
    */
  object MassConversions {
    lazy val nanogram = Nanograms(1)
    lazy val microgram = Micrograms(1)
    lazy val milligram = Milligrams(1)
    lazy val gram = Grams(1)
    lazy val kilogram = Kilograms(1)
    lazy val tonne = Tonnes(1)
    lazy val ounce = Ounces(1)
    lazy val pound = Pounds(1)
    lazy val kilopound = Kilopounds(1)
    lazy val megapound = Megapounds(1)
    lazy val stone = Stone(1)
    lazy val troyGrain = TroyGrains(1)
    lazy val pennyweight = Pennyweights(1)
    lazy val troyOunce = TroyOunces(1)
    lazy val troyPound = TroyPounds(1)
    lazy val tola = Tolas(1)
    lazy val carat = Carats(1)
    lazy val solarMass = SolarMasses(1)

    lazy val eV = ElectronVoltMass(1)
    lazy val meV = MilliElectronVoltMass(1)
    lazy val keV = KiloElectronVoltMass(1)
    lazy val MeV = MegaElectronVoltMass(1)
    lazy val GeV = GigaElectronVoltMass(1)
    lazy val TeV = TeraElectronVoltMass(1)
    lazy val PeV = PetaElectronVoltMass(1)
    lazy val EeV = ExaElectronVoltMass(1)

    lazy val slug = Slugs(1)

    implicit class MassConversions[A](a: A)(implicit n: Numeric[A]) {
      def ng = Nanograms(a)
      def nanograms = ng
      def mcg = Micrograms(a)
      def micrograms = mcg
      def mg = Milligrams(a)
      def milligrams = mg
      def g = Grams(a)
      def grams = g
      def kg = Kilograms(a)
      def kilograms = kg
      def tonnes = Tonnes(a)
      def ounces = Ounces(a)
      def pounds = Pounds(a)
      def kilopounds = Kilopounds(a)
      def megapounds = Megapounds(a)
      def stone = Stone(a)
      def troyGrains = TroyGrains(a)
      def dwt = Pennyweights(a)
      def pennyweights = Pennyweights(a)
      def troyOunces = TroyOunces(a)
      def troyPounds = TroyPounds(a)
      def tolas = Tolas(a)
      def ct = Carats(a)
      def carats = Carats(a)
      def solarMasses = SolarMasses(a)

      def eV = ElectronVoltMass(a)
      def meV = MilliElectronVoltMass(a)
      def keV = KiloElectronVoltMass(a)
      def MeV = MegaElectronVoltMass(a)
      def GeV = GigaElectronVoltMass(a)
      def TeV = TeraElectronVoltMass(a)
      def PeV = PetaElectronVoltMass(a)
      def EeV = ExaElectronVoltMass(a)
      def slugs = Slugs(a)
    }

    implicit class MassStringConversions(val s: String) {
      def toMass = Mass(s)
    }
  }
}


