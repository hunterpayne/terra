/*                                                                      *\
** Squants                                                              **
**                                                                      **
** Scala Quantities and Units of Measure Library and DSL                **
** (c) 2013-2015, Gary Keorkunian                                       **
**                                                                      **
\*                                                                      */

package org.terra
package electro

import scala.reflect.ClassTag

import time.TimeIntegral
import energy.EnergyLike
import space.{ AreaLike, VolumeLike, LengthLike }
import mass.MassLike

/**
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value value in [[org.terra.electro.Coulombs]]
 */
final class ElectricChargeLike[C <: TypeContext](
  val value: C#T, val unit: ElectricChargeUnit[C])(
  implicit ops: TerraOps[C])
    extends Quantity[ElectricChargeLike[C], C#T, C]
    with TimeIntegral[ElectricCurrentLike[C], C#T, C#T, C] {

  import ops.electricChargeOps._
  import ops.electricCurrentOps.Amperes
  import ops.timeOps.Seconds
  import ops.energyOps.Joules
  import ops.capacitanceOps.Farads
  import ops.electricPotentialOps.Volts
  import ops.electricChargeMassRatioOps.CoulombsPerKilogram
  import ops.electricChargeDensityOps.CoulombsPerCubicMeter
  import ops.areaElectricChargeDensityOps.CoulombsPerSquareMeter
  import ops.linearElectricChargeDensityOps.CoulombsPerMeter

  type ElectricPotential = ElectricPotentialLike[C]
  type Capacitance = CapacitanceLike[C]
  type Energy = EnergyLike[C]
  type Length = LengthLike[C]
  type Area = AreaLike[C]
  type Volume = VolumeLike[C]
  type Mass = MassLike[C]
  type LinearElectricChargeDensity = LinearElectricChargeDensityLike[C]
  type AreaElectricChargeDensity = AreaElectricChargeDensityLike[C]
  type ElectricChargeDensity = ElectricChargeDensityLike[C]
  type ElectricChargeMassRatio = ElectricChargeMassRatioLike[C]

  def dimension: Dimension[ElectricChargeLike[C], C#T, C] = 
    ops.electricChargeOps.ElectricCharge

  protected def timeDerived = Amperes(toCoulombs)
  protected def time = Seconds(1)

  def *(that: ElectricPotential)(implicit ops: TerraOps[C]): Energy =
    Joules(num.times(this.toCoulombs, that.toVolts))
  def /(that: ElectricPotential)(implicit ops: TerraOps[C]): Capacitance = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Farads(ops.div[C#T](this.toCoulombs, that.toVolts))
  }
  def /(that: Capacitance)(implicit ops: TerraOps[C]): ElectricPotential = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    Volts(ops.div[C#T](this.toCoulombs, that.toFarads))
  }
  def /(that: Length)(implicit ops: TerraOps[C]): LinearElectricChargeDensity ={
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CoulombsPerMeter(ops.div[C#T](this.toCoulombs, that.toMeters))
  }
  def /(that: Area)(implicit ops: TerraOps[C]): AreaElectricChargeDensity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CoulombsPerSquareMeter(ops.div[C#T](this.toCoulombs, that.toSquareMeters))
  }
  def /(that: Volume)(implicit ops: TerraOps[C]): ElectricChargeDensity = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CoulombsPerCubicMeter(ops.div[C#T](this.toCoulombs, that.toCubicMeters))
  }
  def /(that: Mass)(implicit ops: TerraOps[C]): ElectricChargeMassRatio = {
    implicit val e: HasEnsureType[C#T] = ops.converters.ensureT
    CoulombsPerKilogram(ops.div[C#T](this.toCoulombs, that.toKilograms))
  }

  def toCoulombs = to(Coulombs)
  def toPicocoulombs = to(Picocoulombs)
  def toNanocoulombs = to(Nanocoulombs)
  def toMicrocoulombs = to(Microcoulombs)
  def toMillcoulombs = to(Millicoulombs)
  def toAbcoulombs = to(Abcoulombs)
  def toAmpereHours = to(AmpereHours)
  def toMilliampereHours = to(MilliampereHours)
  def toMilliampereSeconds = to(MilliampereSeconds)
}

trait ElectricChargeUnit[C <: TypeContext] 
    extends UnitOfMeasure[ElectricChargeLike[C], C#T, C] 
    with UnitConverter[C#T, C] {
  def apply(t: C#T)(implicit tag: ClassTag[C#T], ops: TerraOps[C]) = 
    new ElectricChargeLike[C](t, this)
}

trait ElectricChargeOps[C <: TypeContext] {

  implicit val num: Numeric[C#T]
  implicit val ops: TerraOps[C]

  trait ElectricChargeUnitT extends ElectricChargeUnit[C]

  object ElectricCharge extends Dimension[ElectricChargeLike[C], C#T, C] {
    private[electro] def apply[A](a: A, unit: ElectricChargeUnit[C])(
      implicit num: Numeric[A]) = 
      new ElectricChargeLike[C](ops.convDouble(num.toDouble(a)), unit)
    def apply(value: Any) = parse(value)
    def name = "ElectricCharge"
    def primaryUnit = Coulombs
    def siUnit = Coulombs
    def units = Set(Coulombs, Picocoulombs, Nanocoulombs, Microcoulombs, Millicoulombs, Abcoulombs,
      AmpereHours, MilliampereHours, MilliampereSeconds)
  }


  object Coulombs extends ElectricChargeUnitT with PrimaryUnit[C#T, C] 
      with SiUnit {
    val symbol = "C"
  }

  object Picocoulombs extends ElectricChargeUnitT with SiUnit {
    val symbol = "pC"
    val conversionFactor = MetricSystem.Pico
  }

  object Nanocoulombs extends ElectricChargeUnitT with SiUnit {
    val symbol = "nC"
    val conversionFactor = MetricSystem.Nano
  }

  object Microcoulombs extends ElectricChargeUnitT with SiUnit {
    val symbol = "µC"
    val conversionFactor = MetricSystem.Micro
  }

  object Millicoulombs extends ElectricChargeUnitT with SiUnit {
    val symbol = "mC"
    val conversionFactor = MetricSystem.Milli
  }

  object Abcoulombs extends ElectricChargeUnitT {
    val symbol = "aC"
    val conversionFactor = 10d
  }

  object AmpereHours extends ElectricChargeUnitT {
    val symbol = "Ah"
    val conversionFactor = ops.timeOps.Time.SecondsPerHour
  }

  object MilliampereHours extends ElectricChargeUnitT {
    val symbol = "mAh"
    val conversionFactor = AmpereHours.conversionFactor * MetricSystem.Milli
  }

  object MilliampereSeconds extends ElectricChargeUnitT {
    val symbol = "mAs"
    val conversionFactor = Coulombs.conversionFactor * MetricSystem.Milli
  }

  object ElectricChargeConversions {
    lazy val coulomb = Coulombs(1)
    lazy val picocoulomb = Picocoulombs(1)
    lazy val nanocoulomb = Nanocoulombs(1)
    lazy val microcoulomb = Microcoulombs(1)
    lazy val millicoulomb = Millicoulombs(1)
    lazy val abcoulomb = Abcoulombs(1)
    lazy val ampereHour = AmpereHours(1)
    lazy val milliampereHour = MilliampereHours(1)
    lazy val milliampereSecond = MilliampereSeconds(1)

    implicit class ElectricChargeConversions[A](a: A)(implicit num: Numeric[A]) {
      def coulombs = Coulombs(a)
      def picocoulombs = Picocoulombs(a)
      def nanocoulombs = Nanocoulombs(a)
      def microcoulombs = Microcoulombs(a)
      def millicoulombs = Millicoulombs(a)
      def abcoulombs = Abcoulombs(a)
      def ampereHours = AmpereHours(a)
      def milliampereHours = MilliampereHours(a)
      def milliampereSeconds = MilliampereSeconds(a)
    }
  }
}

