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

import electro.{ MagneticFluxLike, MagneticFluxDensityLike }
import mass.{ AreaDensityLike, MassLike }
import photo.{ LuminanceLike, IlluminanceLike, LuminousIntensityLike, LuminousFluxLike }
import energy.PowerLike
import motion.{ PressureLike, ForceLike }
import radio._
import time.TimeLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.space.SquareMeters]]
 */
final class AreaLike[C <: TypeContext](val value: C#T, val unit: AreaUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[AreaLike[C], C#T, C] {

  import ops.areaOps._
  import ops.volumeOps.{ CubicUsMiles, CubicYards, CubicFeet, CubicInches, CubicMeters }
  import ops.massOps.Kilograms
  import ops.forceOps.Newtons
  import ops.luminousFluxOps.Lumens
  import ops.luminousIntensityOps.Candelas
  import ops.magneticFluxOps.Webers
  import ops.powerOps.Watts
  import ops.radiantIntensityOps.WattsPerSteradian
  import ops.areaTimeOps.SquareMeterSeconds
  import ops.lengthOps.{ Meters, UsMiles, Yards, Feet, Inches }

  type Length = LengthLike[C]
  type Volume = VolumeLike[C]
  type Pressure = PressureLike[C]
  type Mass = MassLike[C]
  type Force = ForceLike[C]
  type Luminance = LuminanceLike[C]
  type Illuminance = IlluminanceLike[C]
  type LuminousIntensity = LuminousIntensityLike[C]
  type LuminousFlux = LuminousFluxLike[C]
  type Irradiance = IrradianceLike[C]
  type Power = PowerLike[C]
  type Time = TimeLike[C]
  type AreaTime = AreaTimeLike[C]
  type AreaDensity = AreaDensityLike[C]
  type RadiantIntensity = RadiantIntensityLike[C]
  type MagneticFluxDensity = MagneticFluxDensityLike[C]
  type MagneticFlux = MagneticFluxLike[C]
  type Radiance = RadianceLike[C]

  def dimension: Dimension[AreaLike[C], C#T, C] = Area

  def *(that: Length): Volume = {
    implicit val opsArg = ops
    unit match {
      case SquareUsMiles ⇒ CubicUsMiles(ops.num.times(this.value, that.toUsMiles))
      case SquareYards   ⇒ CubicYards(ops.num.times(this.value, that.toYards))
      case SquareFeet    ⇒ CubicFeet(ops.num.times(this.value, that.toFeet))
      case SquareInches  ⇒ CubicInches(ops.num.times(this.value, that.toInches))
      case _             ⇒ CubicMeters(ops.num.times(this.toSquareMeters, that.toMeters))
    }
  }

  def *(that: AreaDensity)(implicit ops: TerraOps[C]): Mass = {
    Kilograms(ops.num.times(this.toSquareMeters, that.toKilogramsPerSquareMeter))
  }
  def *(that: Pressure)(implicit ops: TerraOps[C]): Force = 
    Newtons(ops.num.times(this.toSquareMeters, that.toPascals))
  def *(that: Illuminance)(implicit ops: TerraOps[C]): LuminousFlux = 
    Lumens(ops.num.times(this.toSquareMeters, that.toLux))
  def *(that: Luminance)(implicit ops: TerraOps[C]): LuminousIntensity = 
    Candelas(ops.num.times(this.toSquareMeters, that.toCandelasPerSquareMeters))
  def *(that: MagneticFluxDensity)(implicit ops: TerraOps[C]): MagneticFlux = 
    Webers(ops.num.times(this.toSquareMeters, that.toTeslas))
  def *(that: Irradiance)(implicit ops: TerraOps[C]): Power = 
    Watts(ops.num.times(this.toSquareMeters, that.toWattsPerSquareMeter))
  def *(that: Radiance)(implicit ops: TerraOps[C]): RadiantIntensity = 
    WattsPerSteradian(ops.num.times(this.toSquareMeters, that.toWattsPerSteradianPerSquareMeter))
  def *(that: Time)(implicit ops: TerraOps[C]): AreaTime = 
    SquareMeterSeconds(ops.num.times(this.toSquareMeters, ops.rconvT(that.toSeconds)))

  def /(that: Length)(implicit ops: TerraOps[C]): Length = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    unit match {
      case SquareUsMiles ⇒ UsMiles(ops.div[C#T](this.value, that.toUsMiles))
      case SquareYards   ⇒ Yards(ops.div[C#T](this.value, that.toYards))
      case SquareFeet    ⇒ Feet(ops.div[C#T](this.value, that.toFeet))
      case SquareInches  ⇒ Inches(ops.div[C#T](this.value, that.toInches))
      case _             ⇒ Meters(ops.div[C#T](this.toSquareMeters, that.toMeters))
    }
  }

  def squareRoot = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Meters(ops.sqrtT[C#T](toSquareMeters))
  }

  def toSquareMeters = to(SquareMeters)
  def toSquareCentimeters = to(SquareCentimeters)
  def toSquareKilometers = to(SquareKilometers)
  def toSquareUsMiles = to(SquareUsMiles)
  def toSquareYards = to(SquareYards)
  def toSquareFeet = to(SquareFeet)
  def toSquareInches = to(SquareInches)
  def toHectares = to(Hectares)
  def toAcres = to(Acres)
  def toBarnes = to(Barnes)
}

trait AreaUnit[C <: TypeContext] extends UnitOfMeasure[AreaLike[C], C#T, C]
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new AreaLike[C](t, this)
}

trait AreaOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait AreaUnitT extends AreaUnit[C]

  object Area extends Dimension[AreaLike[C], C#T, C] {
    private[space] def apply[A](a: A, unit: AreaUnit[C])(
      implicit n: Numeric[A]) = 
      new AreaLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Area"
    def primaryUnit = SquareMeters
    def siUnit = SquareMeters
    def units = Set(SquareMeters, SquareCentimeters, SquareKilometers,
      SquareUsMiles, SquareYards, SquareFeet, SquareInches,
      Hectares, Acres, Barnes)
  }


  object SquareMeters extends AreaUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = "m²"
  }

  object SquareCentimeters extends AreaUnitT with SiUnit {
    val symbol = "cm²"
    val conversionFactor = MetricSystem.Centi * MetricSystem.Centi
  }

  object SquareKilometers extends AreaUnitT with SiUnit {
    val symbol = "km²"
    val conversionFactor = MetricSystem.Kilo * MetricSystem.Kilo
  }

  object SquareUsMiles extends AreaUnitT {
    val symbol = "mi²"
    val conversionFactor = 2.589988110336 * SquareKilometers.conversionFactor
  }

  object SquareYards extends AreaUnitT {
    val symbol = "yd²"
    val conversionFactor = 8.3612736e-1
  }

  object SquareFeet extends AreaUnitT {
    val symbol = "ft²"
    val conversionFactor = 9.290304e-2
  }

  object SquareInches extends AreaUnitT {
    val symbol = "in²"
    val conversionFactor = 6.4516 * SquareCentimeters.conversionFactor
  }

  object Hectares extends AreaUnitT {
    val symbol = "ha"
    val conversionFactor = 10000d
  }

  object Acres extends AreaUnitT {
    val symbol = "acre"
    val conversionFactor = 43560d * SquareFeet.conversionFactor
  }

  object Barnes extends AreaUnitT {
    val symbol = "b"
    val conversionFactor = scala.math.pow(10, -28)
  }

  object AreaConversions {
    lazy val squareMeter = SquareMeters(1)
    lazy val squareCentimeter = SquareCentimeters(1)
    lazy val squareKilometer = SquareKilometers(1)
    lazy val squareMile = SquareUsMiles(1)
    lazy val squareYard = SquareYards(1)
    lazy val squareFoot = SquareFeet(1)
    lazy val squareInch = SquareInches(1)
    lazy val hectare = Hectares(1)
    lazy val acre = Acres(1)
    lazy val barn = Barnes(1)

    implicit class AreaConversions[A](a: A)(implicit n: Numeric[A]) {
      def squareMeters = SquareMeters(a)
      def squareCentimeters = SquareCentimeters(a)
      def squareKilometers = SquareKilometers(a)
      def squareMiles = SquareUsMiles(a)
      def squareYards = SquareYards(a)
      def squareFeet = SquareFeet(a)
      def squareInches = SquareInches(a)
      def hectares = Hectares(a)
      def acres = Acres(a)
      def barnes = Barnes(a)
    }
  }
}

