
package org.terra

import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode
import scala.reflect.ClassTag

import time._
import information._
import electro._
import energy._
import mass._
import motion._
import photo._
import radio._
import space._
import thermal._
import market._

trait TypeContext {
  type T
  type TL
  type TC
  type TT
}

trait HasConverter[I, O] {
  def conv(in: I): O
}

trait HasEnsureType[O] {
  def ensureType(in: Any): O
}

trait Converters[C <: TypeContext] {

  type T = C#T // double
  type TL = C#TL // long
  type TC = C#TC // currency type
  type TT = C#TT // time type

  implicit val ttlConverter: HasConverter[T, TL]
  implicit val tttConverter: HasConverter[T, TT]
  implicit val ttcConverter: HasConverter[T, TC]
  implicit val tltConverter: HasConverter[TL, T]
  implicit val tlttConverter: HasConverter[TL, TT]
  implicit val tltcConverter: HasConverter[TL, TC]
  implicit val tttlConverter: HasConverter[TT, TL]
  implicit val tttConverter2: HasConverter[TT, T]
  implicit val tttcConverter: HasConverter[TT, TC]
  implicit val tctConverter: HasConverter[TC, T]
  implicit val tctlConverter: HasConverter[TC, TL]
  implicit val tcttConverter: HasConverter[TC, TT]

  implicit val dtConverter: HasConverter[Double, T]
  implicit val dtlConverter: HasConverter[Double, TL]
  implicit val dttConverter: HasConverter[Double, TT]
  implicit val dtcConverter: HasConverter[Double, TC]

  implicit val ltConverter: HasConverter[Long, T]
  implicit val ltlConverter: HasConverter[Long, TL]
  implicit val lttConverter: HasConverter[Long, TT]
  implicit val ltcConverter: HasConverter[Long, TC]
  implicit val bdtcConverter: HasConverter[BigDecimal, TC]

  implicit val ensureT: HasEnsureType[T]
  implicit val ensureTL: HasEnsureType[TL]
  implicit val ensureTT: HasEnsureType[TT]
  implicit val ensureTC: HasEnsureType[TC]
}

// context binding together all the scopes for one type
trait TerraOps[C <: TypeContext] {

  type T = C#T // double
  type TL = C#TL // long
  type TC = C#TC // currency type
  type TT = C#TT // time type

  implicit val num: Numeric[T]
  implicit val numL: Numeric[TL]
  implicit val numC: Numeric[TC]
  implicit val numT: Numeric[TT]
  implicit val ops: TerraOps[C] = this

  def nt[T1](implicit tag: ClassTag[T1]): Numeric[T1]
  def makeEnsureType[T1](test: Any): HasEnsureType[T1]

  def getClassTagT: ClassTag[T]
  def getClassTagTL: ClassTag[TL]
  def getClassTagTT: ClassTag[TT]
  def getClassTagTC: ClassTag[TC]

  /*
  trait Converters {
    implicit val ttlConverter: HasConverter[T, TL]
    implicit val tttConverter: HasConverter[T, TT]
    implicit val ttcConverter: HasConverter[T, TC]
    implicit val tltConverter: HasConverter[TL, T]
    implicit val tlttConverter: HasConverter[TL, TT]
    implicit val tltcConverter: HasConverter[TL, TC]
    implicit val tttlConverter: HasConverter[TT, TL]
    implicit val tttConverter2: HasConverter[TT, T]
    implicit val tttcConverter: HasConverter[TT, TC]
    implicit val tctConverter: HasConverter[TC, T]
    implicit val tctlConverter: HasConverter[TC, TL]
    implicit val tcttConverter: HasConverter[TC, TT]

    implicit val dtConverter: HasConverter[Double, T]
    implicit val dtlConverter: HasConverter[Double, TL]
    implicit val dttConverter: HasConverter[Double, TT]
    implicit val dtcConverter: HasConverter[Double, TC]

    implicit val ltConverter: HasConverter[Long, T]
    implicit val ltlConverter: HasConverter[Long, TL]
    implicit val lttConverter: HasConverter[Long, TT]
    implicit val ltcConverter: HasConverter[Long, TC]
    implicit val bdtcConverter: HasConverter[BigDecimal, TC]

    implicit val ensureT: HasEnsureType[T]
    implicit val ensureTL: HasEnsureType[TL]
    implicit val ensureTT: HasEnsureType[TT]
    implicit val ensureTC: HasEnsureType[TC]
  }
*/

  // scope to import containing the implicitly defined HasConverter
  // definations
  val converters: Converters[C]

  def gconvT[O](t: T)(implicit e: HasConverter[T, O]): O =
    gconvTotal[T, O](t)
  def gconvTL[O](tl: TL)(implicit e: HasConverter[TL, O]): O =
    gconvTotal[TL, O](tl)
  def gconvTT[O](tt: TT)(implicit e: HasConverter[TT, O]): O =
    gconvTotal[TT, O](tt)
  def gconvTC[O](tc: TC)(implicit e: HasConverter[TC, O]): O =
    gconvTotal[TC, O](tc)
  def gconvTotal[I, O](in: I)(implicit e: HasConverter[I, O]): O =
    implicitly[HasConverter[I, O]].conv(in)
  def ensureType[O](in: Any)(implicit e: HasEnsureType[O]): O =
    implicitly[HasEnsureType[O]].ensureType(in)

  def convDouble(d: Double)(implicit ops: TerraOps[C]): T = {
    implicit val hasConv = converters.dtConverter
    gconvTotal[Double, T](d)
  }
  def convLong(l: Long)(implicit ops: TerraOps[C]): TL = {
    implicit val hasConv = converters.ltlConverter
    gconvTotal[Long, TL](l)
  }
  def convTime(t: Any)(implicit ops: TerraOps[C]): TT
  def convCurrency(t: Any)(implicit ops: TerraOps[C]): TC

  def conv(t: T): TL = t.asInstanceOf[TL]
  def rconv(t: TL): T = t.asInstanceOf[T]
  def convL(t: TT): TL = t.asInstanceOf[TL]
  def rconvL(t: TL): TT = t.asInstanceOf[TT]
  def convT(t: T): TT = t.asInstanceOf[TT]
  def rconvT(t: TT): T = t.asInstanceOf[T]

  val dimensionlessOps: DimensionlessOps[C]

  val informationOps: InformationOps[C]
  val dataRateOps: DataRateOps[C]

  val timeOps: TimeOps[C]
  val timeSquaredOps: TimeSquaredOps[C]
  val frequencyOps: FrequencyOps[C]

  val areaElectricChargeDensityOps: AreaElectricChargeDensityOps[C]
  val capacitanceOps: CapacitanceOps[C]
  val conductivityOps: ConductivityOps[C]
  val electricChargeOps: ElectricChargeOps[C]
  val electricChargeDensityOps: ElectricChargeDensityOps[C]
  val electricChargeMassRatioOps: ElectricChargeMassRatioOps[C]
  val electricCurrentOps: ElectricCurrentOps[C]
  val electricCurrentDensityOps: ElectricCurrentDensityOps[C]
  val electricFieldStrengthOps: ElectricFieldStrengthOps[C]
  val electricPotentialOps: ElectricPotentialOps[C]
  val electricalConductanceOps: ElectricalConductanceOps[C]
  val electricalResistanceOps: ElectricalResistanceOps[C]
  val inductanceOps: InductanceOps[C]
  val linearElectricChargeDensityOps: LinearElectricChargeDensityOps[C]
  val magneticFieldStrengthOps: MagneticFieldStrengthOps[C]
  val magneticFluxOps: MagneticFluxOps[C]
  val magneticFluxDensityOps: MagneticFluxDensityOps[C]
  val permeabilityOps: PermeabilityOps[C]
  val permittivityOps: PermittivityOps[C]
  val resistivityOps: ResistivityOps[C]

  val energyOps: EnergyOps[C]
  val energyDensityOps: EnergyDensityOps[C]
  val molarEnergyOps: MolarEnergyOps[C]
  val powerOps: PowerOps[C]
  val powerDensityOps: PowerDensityOps[C]
  val powerRampOps: PowerRampOps[C]
  val specificEnergyOps: SpecificEnergyOps[C]

  val areaDensityOps: AreaDensityOps[C]
  val chemicalAmountOps: ChemicalAmountOps[C]
  val densityOps: DensityOps[C]
  val massOps: MassOps[C]
  val momentOfInertiaOps: MomentOfInertiaOps[C]

  val accelerationOps: AccelerationOps[C]
  val angularAccelerationOps: AngularAccelerationOps[C]
  val angularVelocityOps: AngularVelocityOps[C]
  val forceOps: ForceOps[C]
  val jerkOps: JerkOps[C]
  val massFlowOps: MassFlowOps[C]
  val momentumOps: MomentumOps[C]
  val pressureOps: PressureOps[C]
  val pressureChangeOps: PressureChangeOps[C]
  val torqueOps: TorqueOps[C]
  val velocityOps: VelocityOps[C]
  val volumeFlowOps: VolumeFlowOps[C]
  val yankOps: YankOps[C]

  val illuminanceOps: IlluminanceOps[C]
  val luminanceOps: LuminanceOps[C]
  val luminousEnergyOps: LuminousEnergyOps[C]
  val luminousExposureOps: LuminousExposureOps[C]
  val luminousFluxOps: LuminousFluxOps[C]
  val luminousIntensityOps: LuminousIntensityOps[C]

  val activityOps: ActivityOps[C]
  val areaTimeOps: AreaTimeOps[C]
  val doseOps: DoseOps[C]
  val irradianceOps: IrradianceOps[C]
  val particleFluxOps: ParticleFluxOps[C]
  val radianceOps: RadianceOps[C]
  val radiantIntensityOps: RadiantIntensityOps[C]
  val spectralIntensityOps: SpectralIntensityOps[C]
  val spectralIrradianceOps: SpectralIrradianceOps[C]
  val spectralPowerOps: SpectralPowerOps[C]

  val angleOps: AngleOps[C]
  val areaOps: AreaOps[C]
  val lengthOps: LengthOps[C]
  val solidAngleOps: SolidAngleOps[C]
  val volumeOps: VolumeOps[C]

  val temperatureOps: TemperatureOps[C]
  val thermalCapacityOps: ThermalCapacityOps[C]

  val moneyOps: MoneyOps[C]

  def div[T](dividend: T, divisor: T)(
    implicit e: HasEnsureType[T], tag: ClassTag[T]): T
  def mod[T](dividend: T, divisor: T)(
    implicit e: HasEnsureType[T], tag: ClassTag[T]): T
  def floorT[T](t: T)(implicit e: HasEnsureType[T]): T
  def ceilT[T](t: T)(implicit e: HasEnsureType[T]): T
  def rintT[T](t: T)(implicit e: HasEnsureType[T]): T
  def roundT[T](
    t: T, scale: Int, mode: RoundingMode = RoundingMode.HALF_EVEN)(
    implicit e: HasEnsureType[T]): T
  def sqrtT[T](t: T)(implicit e: HasEnsureType[T]): T
  def cbrtT[T](t: T)(implicit e: HasEnsureType[T]): T
  def sinT[T](t: T)(implicit e: HasEnsureType[T]): T
  def cosT[T](t: T)(implicit e: HasEnsureType[T]): T
  def tanT[T](t: T)(implicit e: HasEnsureType[T]): T
  def asinT[T](t: T)(implicit e: HasEnsureType[T]): T
  def acosT[T](t: T)(implicit e: HasEnsureType[T]): T
  def atanT[T](t: T)(implicit e: HasEnsureType[T]): T
  def atan2T[T](y: T, x: T)(implicit e: HasEnsureType[T]): T
}
