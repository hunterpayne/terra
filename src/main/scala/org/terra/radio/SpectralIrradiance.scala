/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package radio

/**
 * @author  florianNussberger
 * @since   0.6
 *
 * @param value Double
 */
final class SpectralIrradianceLike[C <: TypeContext](
  val value: C#T, val unit: SpectralIrradianceUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[SpectralIrradianceLike[C], C#T, C] {

  import ops.spectralIrradianceOps._

  def dimension: Dimension[SpectralIrradianceLike[C], C#T, C] =
    SpectralIrradiance

  def toWattsPerCubicMeter = to(WattsPerCubicMeterIrradiance)
  def toWattsPerSquareMeterPerNanometer = to(WattsPerSquareMeterPerNanometer)
  def toWattsPerSquareMeterPerMicron = to(WattsPerSquareMeterPerMicron)
  def toErgsPerSecondPerSquareCentimeterPerAngstrom = 
    to(ErgsPerSecondPerSquareCentimeterPerAngstrom)
}

trait SpectralIrradianceUnit[C <: TypeContext]
    extends UnitOfMeasure[SpectralIrradianceLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = 
    new SpectralIrradianceLike[C](t, this)
}

trait SpectralIrradianceOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait SpectralIrradianceUnitT extends SpectralIrradianceUnit[C]

  object SpectralIrradiance 
      extends Dimension[SpectralIrradianceLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: SpectralIrradianceUnit[C])(
      implicit n: Numeric[A], ops: TerraOps[C]) =
      new SpectralIrradianceLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "SpectralIrradiance"
    def primaryUnit = WattsPerCubicMeterIrradiance
    def siUnit = WattsPerCubicMeterIrradiance
    def units = Set(WattsPerCubicMeterIrradiance, WattsPerSquareMeterPerMicron, WattsPerSquareMeterPerNanometer, ErgsPerSecondPerSquareCentimeterPerAngstrom)
  }

  import ops.powerOps.{ Watts, ErgsPerSecond }
  import ops.solidAngleOps.SquareRadians
  import ops.volumeOps.CubicMeters
  import ops.areaOps.{ SquareMeters, SquareCentimeters }
  import ops.lengthOps.{ Meters, Nanometers, Microns, Angstroms }

  object WattsPerCubicMeterIrradiance extends SpectralIrradianceUnitT
      with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = Watts.symbol + "/" + CubicMeters.symbol
  }

  object WattsPerSquareMeterPerNanometer 
      extends SpectralIrradianceUnitT with SiUnit {
    val conversionFactor = 1 / MetricSystem.Nano
    val symbol = Watts.symbol + "/" + SquareMeters.symbol + "/" + Nanometers.symbol
  }

  object WattsPerSquareMeterPerMicron extends SpectralIrradianceUnitT
      with SiUnit {
    val conversionFactor = 1 / MetricSystem.Micro
    val symbol = Watts.symbol + "/" + SquareMeters.symbol + "/" + Microns.symbol
  }

  object ErgsPerSecondPerSquareCentimeterPerAngstrom 
      extends SpectralIrradianceUnitT {
    val conversionFactor = ErgsPerSecond.conversionFactor / SquareCentimeters.conversionFactor / Angstroms.conversionFactor
    val symbol = ErgsPerSecond.symbol + "/" + SquareCentimeters.symbol + "/" + Angstroms.symbol
  }

  object SpectralIrradianceConversions {
    lazy val wattPerCubicMeter = WattsPerCubicMeterIrradiance(1)
    lazy val wattPerSquareMeterPerNanometer = WattsPerSquareMeterPerNanometer(1)
    lazy val wattPerSquareMeterPerMicron = WattsPerSquareMeterPerMicron(1)
    lazy val ergPerSecondPerSquareCentimeterPerAngstrom = 
      ErgsPerSecondPerSquareCentimeterPerAngstrom(1)

    implicit class SpectralIrradianceConversions[A](a: A)(
      implicit n: Numeric[A]) {
      def wattsPerCubicMeter = WattsPerCubicMeterIrradiance(a)
      def wattsPerSquareMeterPerNanometer = WattsPerSquareMeterPerNanometer(a)
      def wattsPerSquareMeterPerMicron = WattsPerSquareMeterPerMicron(a)
      def ergsPerSecondPerSquareCentimeterPerAngstrom =
        ErgsPerSecondPerSquareCentimeterPerAngstrom(a)
    }
  }
}

