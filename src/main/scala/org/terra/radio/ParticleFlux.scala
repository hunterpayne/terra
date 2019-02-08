/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2018, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package radio

import scala.reflect.ClassTag

import energy.EnergyLike

/**
 * @author  Hunter Payne
 *
 * @param value Double
 */
final class ParticleFluxLike[C <: TypeContext](
  val value: C#T, val unit: ParticleFluxUnit[C])(implicit ops: TerraOps[C])
    extends Quantity[ParticleFluxLike[C], C#T, C] {

  import ops.particleFluxOps._
  import ops.activityOps.Becquerels
  import ops.timeOps.Hours
  import ops.irradianceOps.WattsPerSquareMeter

  type AreaTime = AreaTimeLike[C]
  type Activity = ActivityLike[C]
  type Energy = EnergyLike[C]
  type Irradiance = IrradianceLike[C]

  def dimension: Dimension[ParticleFluxLike[C], C#T, C] = ParticleFlux

  def *(that: AreaTime): Activity = {
    implicit val opsArg = ops
    Becquerels(ops.num.times(
      this.toBecquerelsPerSquareMeterSecond, that.toSquareMeterSeconds))
  }
  def *(that: Energy): Irradiance = {
    implicit val opsArg = ops
    WattsPerSquareMeter(
      ops.num.times(
        ops.num.times(ops.rconvT(Hours(1).toSeconds), that.toWattHours),
        this.toBecquerelsPerSquareMeterSecond))
  }

  def toBecquerelsPerSquareMeterSecond = to(BecquerelsPerSquareMeterSecond)
  def toBecquerelsPerSquareCentimeterSecond = 
    to(BecquerelsPerSquareCentimeterSecond)
}

trait ParticleFluxUnit[C <: TypeContext]
    extends UnitOfMeasure[ParticleFluxLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new ParticleFluxLike[C](t, this)
}

trait ParticleFluxOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]
  def convDouble(d: Double)(implicit ops: TerraOps[C]): C#T

  trait ParticleFluxUnitT extends ParticleFluxUnit[C]

  object ParticleFlux extends Dimension[ParticleFluxLike[C], C#T, C] {
    private[radio] def apply[A](a: A, unit: ParticleFluxUnit[C])(
      implicit n: Numeric[A]) =
      new ParticleFluxLike[C](ops.convDouble(n.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ParticleFlux"
    def primaryUnit = BecquerelsPerSquareMeterSecond
    def siUnit = BecquerelsPerSquareMeterSecond
    def units =
      Set(BecquerelsPerSquareMeterSecond, BecquerelsPerSquareCentimeterSecond)
  }

  import ops.activityOps.Becquerels
  import ops.powerOps.Watts

  object BecquerelsPerSquareCentimeterSecond extends ParticleFluxUnitT {
    val conversionFactor = 10000.0 //0.0001
    val symbol = Becquerels.symbol + "/cm²‧s"
  }

  object BecquerelsPerSquareMeterSecond
      extends ParticleFluxUnitT with PrimaryUnit[C#T, C] with SiUnit {
    val symbol = Becquerels.symbol + "/m²‧s"
  }

  object ParticleFluxConversions {
    lazy val becquerelPerSquareMeterSecond = BecquerelsPerSquareMeterSecond(1)
    lazy val becquerelPerSquareCentimeterSecond =
      BecquerelsPerSquareCentimeterSecond(1)

    implicit class ParticleFluxConversions[A](a: A)(
      implicit n: Numeric[A]) {
      def becquerelsPerSquareMeterSecond = BecquerelsPerSquareMeterSecond(a)
      def becquerelsPerSquareCentimeterSecond =
        BecquerelsPerSquareCentimeterSecond(a)
    }
  }
}

