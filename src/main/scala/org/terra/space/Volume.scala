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

import org.terra.energy.{ EnergyLike, EnergyDensityLike }
import org.terra.mass.{ ChemicalAmountLike, MassLike, DensityLike }
import org.terra.motion.VolumeFlowLike
import org.terra.time.TimeIntegral

/**
 * Represents a quantity of Volume (three-dimensional space)
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.space.CubicMeters]]
 */
final class VolumeLike[C <: TypeContext](val value: C#T, val unit: VolumeUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[VolumeLike[C], C#T, C]
    with TimeIntegral[VolumeFlowLike[C], C#T, C#T, C] {

  import ops.volumeOps._
  import ops.timeOps.Seconds
  import ops.volumeFlowOps.CubicMetersPerSecond
  import ops.massOps.Kilograms
  import ops.energyOps.Joules
  import ops.areaOps.{ SquareUsMiles, SquareYards, SquareFeet, SquareInches, SquareMeters }
  import ops.lengthOps.{ UsMiles, Yards, Feet, Inches, Meters }
  import ops.specificVolumeOps.CubicMetersPerKilogram
  import ops.molarVolumeOps.CubicMetersPerMole

  type Mass = MassLike[C]
  type Density = DensityLike[C]
  type Energy = EnergyLike[C]
  type EnergyDensity = EnergyDensityLike[C]
  type Area = AreaLike[C]
  type Length = LengthLike[C]
  type ChemicalAmount = ChemicalAmountLike[C]
  type SpecificVolume = SpecificVolumeLike[C]
  type MolarVolume = MolarVolumeLike[C]

  def dimension: Dimension[VolumeLike[C], C#T, C] = Volume

  protected def timeDerived = CubicMetersPerSecond(toCubicMeters)
  protected[terra] def time = Seconds(1)

  def *(that: Density)(implicit ops: TerraOps[C]): Mass = 
    Kilograms(ops.num.times(this.toCubicMeters, that.toKilogramsPerCubicMeter))
  def *(that: EnergyDensity)(implicit ops: TerraOps[C]): Energy = 
    Joules(ops.num.times(this.toCubicMeters, that.toJoulesPerCubicMeter))

  def /(that: Area)(implicit ops: TerraOps[C]): Length = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    unit match {
      case CubicUsMiles ⇒ UsMiles(ops.div[C#T](this.value, that.toSquareUsMiles))
      case CubicYards   ⇒ Yards(ops.div[C#T](this.value, that.toSquareYards))
      case CubicFeet    ⇒ Feet(ops.div[C#T](this.value, that.toSquareFeet))
      case CubicInches  ⇒ Inches(ops.div[C#T](this.value, that.toSquareInches))
      case _            ⇒ Meters(ops.div[C#T](this.toCubicMeters, that.toSquareMeters))
    }
  }

  def /(that: Length)(implicit ops: TerraOps[C]): Area = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    unit match {
      case CubicUsMiles ⇒ SquareUsMiles(ops.div[C#T](this.value, that.toUsMiles))
      case CubicYards   ⇒ SquareYards(ops.div[C#T](this.value, that.toYards))
      case CubicFeet    ⇒ SquareFeet(ops.div[C#T](this.value, that.toFeet))
      case CubicInches  ⇒ SquareInches(ops.div[C#T](this.value, that.toInches))
      case _            ⇒ SquareMeters(ops.div[C#T](this.toCubicMeters, that.toMeters))
    }
  }

  def /(that: Mass)(implicit ops: TerraOps[C]): SpecificVolume = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CubicMetersPerKilogram(ops.div[C#T](toCubicMeters, that.toKilograms))
  }
  def /(that: ChemicalAmount)(implicit ops: TerraOps[C]): MolarVolume = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CubicMetersPerMole(ops.div[C#T](toCubicMeters, that.toMoles))
  }

  def cubeRoot = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Meters(ops.cbrtT[C#T](toCubicMeters))
  }

  def toCubicMeters = to(CubicMeters)
  def toLitres = to(Litres)
  def toNanolitres = to(Nanolitres)
  def toMicrolitres = to(Microlitres)
  def toMillilitres = to(Millilitres)
  def toCentilitres = to(Centilitres)
  def toDecilitres = to(Decilitres)
  def toHectolitres = to(Hectolitres)

  def toCubicMiles = to(CubicUsMiles)
  def toCubicYards = to(CubicYards)
  def toCubicFeet = to(CubicFeet)
  def toCubicInches = to(CubicInches)

  def toUsGallons = to(UsGallons)
  def toUsQuarts = to(UsQuarts)
  def toUsPints = to(UsPints)
  def toUsCups = to(UsCups)
  def toFluidOunces = to(FluidOunces)
  def toTablespoons = to(Tablespoons)
  def toTeaspoons = to(Teaspoons)

  def toUsDryGallons = to(UsDryGallons)
  def toUsDryQuarts = to(UsDryQuarts)
  def toUsDryPints = to(UsDryPints)
  def toUsDryCups = to(UsDryCups)

  def toImperialGallons = to(ImperialGallons)
  def toImperialQuarts = to(ImperialQuarts)
  def toImperialPints = to(ImperialPints)
  def toImperialCups = to(ImperialCups)

  def toAcreFeet = to(AcreFeet)
}

trait VolumeUnit[C <: TypeContext] extends UnitOfMeasure[VolumeLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) =
    new VolumeLike[C](t, this)
}

trait VolumeOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait VolumeUnitT extends VolumeUnit[C]

  object Volume extends Dimension[VolumeLike[C], C#T, C] {
    private[space] def apply[A](a: A, unit: VolumeUnit[C])(
      implicit n: Numeric[A]) = 
      new VolumeLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Volume"
    def primaryUnit = CubicMeters
    def siUnit = CubicMeters
    def units = Set(CubicMeters, Litres, Nanolitres, Microlitres, Millilitres, Centilitres,
      Decilitres, Hectolitres,
      CubicUsMiles, CubicYards, CubicFeet, CubicInches,
      UsGallons, UsQuarts, UsPints, UsCups, FluidOunces, Tablespoons, Teaspoons,
      AcreFeet)
  }

  object CubicMeters extends VolumeUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "m³"
  }

  object Litres extends VolumeUnitT {
    val symbol = "L"
    val conversionFactor = .001
  }

  object Nanolitres extends VolumeUnitT {
    val symbol = "nl"
    val conversionFactor = Litres.conversionFactor * MetricSystem.Nano
  }

  object Microlitres extends VolumeUnitT {
    val symbol = "µl"
    val conversionFactor = Litres.conversionFactor * MetricSystem.Micro
  }

  object Millilitres extends VolumeUnitT {
    val symbol = "ml"
    val conversionFactor = Litres.conversionFactor * MetricSystem.Milli
  }

  object Centilitres extends VolumeUnitT {
    val symbol = "cl"
    val conversionFactor = Litres.conversionFactor * MetricSystem.Centi
  }

  object Decilitres extends VolumeUnitT {
    val symbol = "dl"
    val conversionFactor = Litres.conversionFactor * MetricSystem.Deci
  }

  object Hectolitres extends VolumeUnitT {
    val symbol = "hl"
    val conversionFactor = Litres.conversionFactor * MetricSystem.Hecto
  }

  import ops.lengthOps.{ UsMiles, Yards, Feet, Inches }

  object CubicUsMiles extends VolumeUnitT {
    val symbol = "mi³"
    val conversionFactor = math.pow(UsMiles.conversionFactor, 3)
  }

  object CubicYards extends VolumeUnitT {
    val symbol = "yd³"
    val conversionFactor = BigDecimal(Yards.conversionFactor).pow(3).toDouble
  }

  object CubicFeet extends VolumeUnitT {
    val symbol = "ft³"
    val conversionFactor = BigDecimal(Feet.conversionFactor).pow(3).toDouble
  }

  object CubicInches extends VolumeUnitT {
    val symbol = "in³"
    val conversionFactor = math.pow(Inches.conversionFactor, 3)
  }

  object UsGallons extends VolumeUnitT {
    val symbol = "gal"
    val conversionFactor = Millilitres.conversionFactor * 3.785411784e3
  }

  object UsQuarts extends VolumeUnitT {
    val symbol = "qt"
    val conversionFactor = UsGallons.conversionFactor / 4d
  }

  object UsPints extends VolumeUnitT {
    val symbol = "pt"
    val conversionFactor = UsGallons.conversionFactor / 8d
  }

  object UsCups extends VolumeUnitT {
    val symbol = "c"
    val conversionFactor = UsGallons.conversionFactor / 16d
  }

  object FluidOunces extends VolumeUnitT {
    val symbol = "oz"
    val conversionFactor = UsGallons.conversionFactor / 128d
  }

  object Tablespoons extends VolumeUnitT {
    val symbol = "tbsp"
    val conversionFactor = FluidOunces.conversionFactor / 2d
  }

  object Teaspoons extends VolumeUnitT {
    val symbol = "tsp"
    val conversionFactor = FluidOunces.conversionFactor / 6d
  }

  object UsDryGallons extends VolumeUnitT {
    val symbol = "gal"
    val conversionFactor = Millilitres.conversionFactor * 4.4048837e3
  }

  object UsDryQuarts extends VolumeUnitT {
    val symbol = "qt"
    val conversionFactor = UsDryGallons.conversionFactor / 4d
  }

  object UsDryPints extends VolumeUnitT {
    val symbol = "pt"
    val conversionFactor = UsDryGallons.conversionFactor / 8d
  }

  object UsDryCups extends VolumeUnitT {
    val symbol = "c"
    val conversionFactor = UsDryGallons.conversionFactor / 16d
  }

  object ImperialGallons extends VolumeUnitT {
    val symbol = "gal"
    val conversionFactor = Millilitres.conversionFactor * 4.54609e3
  }

  object ImperialQuarts extends VolumeUnitT {
    val symbol = "qt"
    val conversionFactor = ImperialGallons.conversionFactor / 4d
  }

  object ImperialPints extends VolumeUnitT {
    val symbol = "pt"
    val conversionFactor = ImperialGallons.conversionFactor / 8d
  }

  object ImperialCups extends VolumeUnitT {
    val symbol = "c"
    val conversionFactor = ImperialGallons.conversionFactor / 16d
  }

  object AcreFeet extends VolumeUnitT {
    val symbol = "acft"
    val conversionFactor = CubicFeet.conversionFactor * 43560d
  }

  object VolumeConversions {
    lazy val cubicMeter = CubicMeters(1)
    lazy val litre = Litres(1)
    lazy val liter = Litres(1)
    lazy val nanolitre = Nanolitres(1)
    lazy val nanoliter = Nanolitres(1)
    lazy val microlitre = Microlitres(1)
    lazy val microliter = Microlitres(1)
    lazy val millilitre = Millilitres(1)
    lazy val milliliter = Millilitres(1)
    lazy val centilitre = Centilitres(1)
    lazy val centiliter = Centilitres(1)
    lazy val decilitre = Decilitres(1)
    lazy val deciliter = Decilitres(1)
    lazy val hectolitre = Hectolitres(1)
    lazy val hectoliter = Hectolitres(1)

    lazy val cubicMile = CubicUsMiles(1)
    lazy val cubicYard = CubicYards(1)
    lazy val cubicFoot = CubicFeet(1)
    lazy val cubicInch = CubicInches(1)

    lazy val gallon = UsGallons(1)
    lazy val quart = UsQuarts(1)
    lazy val pint = UsPints(1)
    lazy val cup = UsCups(1)

    lazy val fluidOunce = FluidOunces(1)
    lazy val tablespoon = Tablespoons(1)
    lazy val teaspoon = Teaspoons(1)

    lazy val acreFoot = AcreFeet(1)

    implicit class VolumeConversions[A](a: A)(implicit n: Numeric[A]) {
      def cubicMeters = CubicMeters(a)
      def cubicMetres = CubicMeters(a)
      def litres = Litres(a)
      def liters = Litres(a)
      def nanolitres = Nanolitres(a)
      def nanoliters = Nanolitres(a)
      def microlitres = Microlitres(a)
      def microliters = Microlitres(a)
      def millilitres = Millilitres(a)
      def milliliters = Millilitres(a)
      def centilitres = Centilitres(a)
      def centiliters = Centilitres(a)
      def decilitres = Decilitres(a)
      def deciliters = Decilitres(a)
      def hectolitres = Hectolitres(a)
      def hectoliters = Hectolitres(a)

      def cubicMiles = CubicUsMiles(a)
      def cubicYards = CubicYards(a)
      def cubicFeet = CubicFeet(a)
      def cubicInches = CubicInches(a)

      def gallons = UsGallons(a)
      def quarts = UsQuarts(a)
      def pints = UsPints(a)
      def cups = UsCups(a)
      def fluidOunces = FluidOunces(a)
      def tablespoons = Tablespoons(a)
      def teaspoons = Teaspoons(a)

      def acreFeet = AcreFeet(a)
    }
  }
}

