/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package radio

import org.terra.energy.{ PowerLike, EnergyLike }
import org.terra.space.AreaLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class IrradianceLike[C <: TypeContext](val value: C#T, val unit: IrradianceUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[IrradianceLike[C], C#T, C] {

  import ops.irradianceOps._
  import ops.powerOps.{ Watts, ErgsPerSecond }
  import ops.energyOps.WattHours
  import ops.timeOps.Hours
  import ops.particleFluxOps.BecquerelsPerSquareMeterSecond

  type Area = AreaLike[C]
  type Power = PowerLike[C]
  type AreaTime = AreaTimeLike[C]
  type Energy = EnergyLike[C]
  type ParticleFlux = ParticleFluxLike[C]

  def dimension: Dimension[IrradianceLike[C], C#T, C] = Irradiance

  def *(that: Area)(implicit ops: TerraOps[C]): Power = 
    Watts(ops.num.times(this.toWattsPerSquareMeter, that.toSquareMeters))
  // the Hours(1).toSeconds is to convert watt hours to watt seconds which 
  // isn't a normal supported type
  def *(that: AreaTime)(implicit ops: TerraOps[C]): Energy = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    WattHours(
      ops.div[C#T](
        ops.num.times(this.toWattsPerSquareMeter, that.toSquareMeterSeconds),
        ops.rconvT(Hours(1).toSeconds)))
  }
  def /(that: Energy)(implicit ops: TerraOps[C]): ParticleFlux = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    BecquerelsPerSquareMeterSecond(
      ops.div[C#T](
        toWattsPerSquareMeter,
        ops.num.times(that.toWattHours, ops.rconvT(Hours(1).toSeconds))))
  }
  def /(that: ParticleFlux)(implicit ops: TerraOps[C]): Energy = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    WattHours(
      ops.div[C#T](
        ops.div[C#T](toWattsPerSquareMeter, that.toBecquerelsPerSquareMeterSecond),
        ops.rconvT(Hours(1).toSeconds)))
  }

  def toWattsPerSquareMeter = to(WattsPerSquareMeter)
  def toErgsPerSecondPerSquareCentimeter = to(ErgsPerSecondPerSquareCentimeter)
}

trait IrradianceUnit[C <: TypeContext] 
    extends UnitOfMeasure[IrradianceLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit ops: TerraOps[C]) = new IrradianceLike[C](t, this)
}

trait IrradianceOps[C <: TypeContext] {

  implicit val ops: TerraOps[C]

  trait IrradianceUnitT extends IrradianceUnit[C]

  object Irradiance extends Dimension[IrradianceLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: IrradianceUnit[C])(
      implicit n: Numeric[A]) = 
      new IrradianceLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "Irradiance"
    def primaryUnit = WattsPerSquareMeter
    def siUnit = WattsPerSquareMeter
    def units = Set(WattsPerSquareMeter, ErgsPerSecondPerSquareCentimeter)
  }

  import ops.powerOps.{ Watts, ErgsPerSecond }
  import ops.areaOps.{ SquareMeters, SquareCentimeters }

  object WattsPerSquareMeter extends IrradianceUnitT with PrimaryUnit[C#T, C]
      with SiUnit {
    val symbol = Watts.symbol + "/" + SquareMeters.symbol
  }

  object ErgsPerSecondPerSquareCentimeter extends IrradianceUnitT {
    val conversionFactor = 
      ErgsPerSecond.conversionFactor / SquareCentimeters.conversionFactor
    val symbol = ErgsPerSecond.symbol + "/" + SquareCentimeters.symbol
  }

  object IrradianceConversions {
    lazy val wattPerSquareMeter = WattsPerSquareMeter(1)
    lazy val ergPerSecondPerSquareCentimeter = 
      ErgsPerSecondPerSquareCentimeter(1)

    implicit class IrradianceConversions[A](a: A)(implicit n: Numeric[A]) {
      def wattsPerSquareMeter = WattsPerSquareMeter(a)
      def ergsPerSecondPerSquareCentimeter = ErgsPerSecondPerSquareCentimeter(a)
    }
  }
}

