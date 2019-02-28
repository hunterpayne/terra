
package org.terra

import scala.util.Try
import scala.math.BigDecimal
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode.RoundingMode

import scala.reflect.{ ClassTag, classTag }

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

/**
  * This is the mega class that brings together all the dimension specific
  * traits (the *Ops traits) into 1 big class that is extended by the
  * specific TerraOps implementions (classes that specify how to perform 
  * mathematical operations).
  */
abstract class AbstractDoubleTerraOps[C <: TypeContext](
  implicit n: Numeric[Double], nl: Numeric[Long], nc: Numeric[BigDecimal])
    extends TerraOps[C]
    with DimensionlessOps[C]
    with InformationOps[C]
    with DataRateOps[C]
    with TimeOps[C]
    with TimeSquaredOps[C]
    with FrequencyOps[C]
    with AreaElectricChargeDensityOps[C]
    with CapacitanceOps[C]
    with ConductivityOps[C]
    with ElectricChargeOps[C]
    with ElectricChargeDensityOps[C]
    with ElectricChargeMassRatioOps[C]
    with ElectricCurrentOps[C]
    with ElectricCurrentDensityOps[C]
    with ElectricFieldStrengthOps[C]
    with ElectricPotentialOps[C]
    with ElectricalConductanceOps[C]
    with ElectricalResistanceOps[C]
    with InductanceOps[C]
    with LinearElectricChargeDensityOps[C]
    with MagneticFieldStrengthOps[C]
    with MagneticFluxOps[C]
    with MagneticFluxDensityOps[C]
    with PermeabilityOps[C]
    with PermittivityOps[C]
    with ResistivityOps[C]
    with EnergyOps[C]
    with EnergyDensityOps[C]
    with EnergyAreaDensityOps[C]
    with MolarEnergyOps[C]
    with PowerOps[C]
    with PowerDensityOps[C]
    with PowerRampOps[C]
    with SpecificEnergyOps[C] 
    with AreaDensityOps[C]
    with ChemicalAmountOps[C]
    with DensityOps[C]
    with MassOps[C]
    with MomentOfInertiaOps[C]
    with MolarMassOps[C]
    with ConcentrationOps[C]
    with CatalyticActivityOps[C]
    with AccelerationOps[C]
    with AngularAccelerationOps[C]
    with AngularVelocityOps[C]
    with ForceOps[C]
    with JerkOps[C]
    with MassFlowOps[C]
    with MomentumOps[C]
    with PressureOps[C]
    with PressureChangeOps[C]
    with TorqueOps[C]
    with VelocityOps[C]
    with VolumeFlowOps[C]
    with YankOps[C]
    with SurfaceTensionOps[C]
    with ViscosityOps[C]
    with IlluminanceOps[C]
    with LuminanceOps[C]
    with LuminousEnergyOps[C]
    with LuminousExposureOps[C]
    with LuminousFluxOps[C]
    with LuminousIntensityOps[C]
    with ActivityOps[C]
    with AreaTimeOps[C]
    with DoseOps[C]
    with IrradianceOps[C]
    with ParticleFluxOps[C]
    with RadianceOps[C]
    with RadiantIntensityOps[C]
    with SpectralIntensityOps[C]
    with SpectralIrradianceOps[C]
    with SpectralPowerOps[C]
    with AbsorbedDoseOps[C]
    with AngleOps[C]
    with AreaOps[C]
    with LengthOps[C]
    with SolidAngleOps[C]
    with VolumeOps[C]
    with MolarVolumeOps[C]
    with SpecificVolumeOps[C]
    with TemperatureOps[C]
    with ThermalCapacityOps[C]
    with MoneyOps[C] 
    with EmployeeOps[C]
    with LaborOps[C] {

  val doubleNumeric: Numeric[Double] = n
  val longNumeric: Numeric[Long] = nl
  val bigDecimalNumeric: Numeric[BigDecimal] = nc
  val DoubleTag = classTag[Double]
  val LongTag = classTag[Long]
  val IntTag = classTag[Int]
  val BigDecimalTag = classTag[BigDecimal]

  assert(DoubleTag != null)
  assert(LongTag != null)
  assert(IntTag != null)
  assert(BigDecimalTag != null)

  def getClassTagT: ClassTag[T] = DoubleTag.asInstanceOf[ClassTag[T]]
  def getClassTagTL: ClassTag[TL] = LongTag.asInstanceOf[ClassTag[TL]]
  def getClassTagTT: ClassTag[TT] = DoubleTag.asInstanceOf[ClassTag[TT]]
  def getClassTagTC: ClassTag[TC] = BigDecimalTag.asInstanceOf[ClassTag[TC]]

  def nt[T1](implicit tag: ClassTag[T1]): Numeric[T1] =
    tag match {
      case DoubleTag => num.asInstanceOf[Numeric[T1]]
      case LongTag => numL.asInstanceOf[Numeric[T1]]
      case BigDecimalTag => numC.asInstanceOf[Numeric[T1]]
      case IntTag => //numL.asInstanceOf[Numeric[T1]]
        Numeric.IntIsIntegral.asInstanceOf[Numeric[T1]]
      case clz if (clz != null) => {
        println("got clz " + clz + " of type " + clz.runtimeClass)
        //assert(nl.isInstanceOf[Numeric[T1]])
        num.asInstanceOf[Numeric[T1]]
      }
      case _ => {
        //(new Exception("null class tag guessing")).printStackTrace()
        println("null class tag guessing")
        num.asInstanceOf[Numeric[T1]]
      }
    }

  def convTime(t: Any)(implicit ops: TerraOps[C]): TT = t match {
    case d: Double => {
      implicit val hasConv = converters.dttConverter
      gconvTotal[Double, TT](d)
    }
    case l: Long => {
      // seems to be necessary to make the compiler happy
      implicit val hasConv = converters.lttConverter
      gconvTotal[Long, TT](l)
    }
    case _ => assert(false); numT.zero
  }

  def convCurrency(t: Any)(implicit ops: TerraOps[C]): TC = t match {
    case d: Double => {
      implicit val hasConv = converters.dtcConverter
      gconvTotal[Double, TC](d)
    }
    case l: Long => {
      implicit val hasConv = converters.ltcConverter
      gconvTotal[Long, TC](l)
    }
    case bd: BigDecimal => {
      implicit val hasConv = converters.bdtcConverter
      gconvTotal[BigDecimal, TC](bd)
    }
    case _ => assert(false); numC.zero
  }

  val dimensionlessOps: DimensionlessOps[C] = this

  val informationOps: InformationOps[C] = this
  val dataRateOps: DataRateOps[C] = this

  val timeOps: TimeOps[C] = this
  val timeSquaredOps: TimeSquaredOps[C] = this
  val frequencyOps: FrequencyOps[C] = this

  val areaElectricChargeDensityOps: AreaElectricChargeDensityOps[C] = this
  val capacitanceOps: CapacitanceOps[C] = this
  val conductivityOps: ConductivityOps[C] = this
  val electricChargeOps: ElectricChargeOps[C] = this
  val electricChargeDensityOps: ElectricChargeDensityOps[C] = this
  val electricChargeMassRatioOps: ElectricChargeMassRatioOps[C] = this
  val electricCurrentOps: ElectricCurrentOps[C] = this
  val electricCurrentDensityOps: ElectricCurrentDensityOps[C] = this
  val electricFieldStrengthOps: ElectricFieldStrengthOps[C] = this
  val electricPotentialOps: ElectricPotentialOps[C] = this
  val electricalConductanceOps: ElectricalConductanceOps[C] = this
  val electricalResistanceOps: ElectricalResistanceOps[C] = this
  val inductanceOps: InductanceOps[C] = this
  val linearElectricChargeDensityOps: LinearElectricChargeDensityOps[C] = this
  val magneticFieldStrengthOps: MagneticFieldStrengthOps[C] = this
  val magneticFluxOps: MagneticFluxOps[C] = this
  val magneticFluxDensityOps: MagneticFluxDensityOps[C] = this
  val permeabilityOps: PermeabilityOps[C] = this
  val permittivityOps: PermittivityOps[C] = this
  val resistivityOps: ResistivityOps[C] = this

  val energyOps: EnergyOps[C] = this
  val energyDensityOps: EnergyDensityOps[C] = this
  val energyAreaDensityOps: EnergyAreaDensityOps[C] = this
  val molarEnergyOps: MolarEnergyOps[C] = this
  val powerOps: PowerOps[C] = this
  val powerDensityOps: PowerDensityOps[C] = this
  val powerRampOps: PowerRampOps[C] = this
  val specificEnergyOps: SpecificEnergyOps[C] = this

  val areaDensityOps: AreaDensityOps[C] = this
  val chemicalAmountOps: ChemicalAmountOps[C] = this
  val densityOps: DensityOps[C] = this
  val massOps: MassOps[C] = this
  val momentOfInertiaOps: MomentOfInertiaOps[C] = this
  val molarMassOps: MolarMassOps[C] = this
  val concentrationOps: ConcentrationOps[C] = this
  val catalyticActivityOps: CatalyticActivityOps[C] = this

  val accelerationOps: AccelerationOps[C] = this
  val angularAccelerationOps: AngularAccelerationOps[C] = this
  val angularVelocityOps: AngularVelocityOps[C] = this
  val forceOps: ForceOps[C] = this
  val jerkOps: JerkOps[C] = this
  val massFlowOps: MassFlowOps[C] = this
  val momentumOps: MomentumOps[C] = this
  val pressureOps: PressureOps[C] = this
  val pressureChangeOps: PressureChangeOps[C] = this
  val torqueOps: TorqueOps[C] = this
  val velocityOps: VelocityOps[C] = this
  val volumeFlowOps: VolumeFlowOps[C] = this
  val yankOps: YankOps[C] = this
  val surfaceTensionOps: SurfaceTensionOps[C] = this
  val viscosityOps: ViscosityOps[C] = this

  val illuminanceOps: IlluminanceOps[C] = this
  val luminanceOps: LuminanceOps[C] = this
  val luminousEnergyOps: LuminousEnergyOps[C] = this
  val luminousExposureOps: LuminousExposureOps[C] = this
  val luminousFluxOps: LuminousFluxOps[C] = this
  val luminousIntensityOps: LuminousIntensityOps[C] = this

  val activityOps: ActivityOps[C] = this
  val areaTimeOps: AreaTimeOps[C] = this
  val doseOps: DoseOps[C] = this
  val irradianceOps: IrradianceOps[C] = this
  val particleFluxOps: ParticleFluxOps[C] = this
  val radianceOps: RadianceOps[C] = this
  val radiantIntensityOps: RadiantIntensityOps[C] = this
  val spectralIntensityOps: SpectralIntensityOps[C] = this
  val spectralIrradianceOps: SpectralIrradianceOps[C] = this
  val spectralPowerOps: SpectralPowerOps[C] = this
  val absorbedDoseOps: AbsorbedDoseOps[C] = this

  val angleOps: AngleOps[C] = this
  val areaOps: AreaOps[C] = this
  val lengthOps: LengthOps[C] = this
  val solidAngleOps: SolidAngleOps[C] = this
  val volumeOps: VolumeOps[C] = this
  val molarVolumeOps: MolarVolumeOps[C] = this
  val specificVolumeOps: SpecificVolumeOps[C] = this

  val temperatureOps: TemperatureOps[C] = this
  val thermalCapacityOps: ThermalCapacityOps[C] = this

  val moneyOps: MoneyOps[C] = this
  val employeeOps: EmployeeOps[C] = this
  val laborOps: LaborOps[C] = this

  def div[T](dividend: T, divisor: T)(
    implicit e: HasEnsureType[T], tag: ClassTag[T]): T =
    dividend match {
      case b: Byte => 
        ensureType[T](b.toDouble / divisor.asInstanceOf[Byte].toDouble)
      case s: Short => 
        ensureType[T](s.toDouble / divisor.asInstanceOf[Short].toDouble)
      case i: Int => 
        ensureType[T](i.toDouble / divisor.asInstanceOf[Int].toDouble)
      case l: Long => {
        implicit val num: Numeric[T] = nt[T]
        ensureType[T](l.toDouble / num.toDouble(divisor))
      }
      case f: Float => ensureType[T](f / divisor.asInstanceOf[Float])
      case d: Double => ensureType[T](d / divisor.asInstanceOf[Double])
      case bi: BigInt => ensureType[T](bi / divisor.asInstanceOf[BigInt])
      case bd: BigDecimal =>
        ensureType[T](bd / divisor.asInstanceOf[BigDecimal])
    }

  def mod[T](dividend: T, divisor: T)(
    implicit e: HasEnsureType[T], tag: ClassTag[T]): T =
    dividend match {
      case b: Byte => (b % divisor.asInstanceOf[Byte]).asInstanceOf[T]
      case s: Short => (s % divisor.asInstanceOf[Short]).asInstanceOf[T]
      case i: Int => (i % divisor.asInstanceOf[Int]).asInstanceOf[T]
      case l: Long => ensureType[T](l % divisor.asInstanceOf[Long])
      case f: Float => (f % divisor.asInstanceOf[Float]).asInstanceOf[T]
      case d: Double => (d % divisor.asInstanceOf[Double]).asInstanceOf[T]
      case bi: BigInt => (bi % divisor.asInstanceOf[BigInt]).asInstanceOf[T]
      case bd: BigDecimal =>
        (bd % divisor.asInstanceOf[BigDecimal]).asInstanceOf[T]
    }

  def floorT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case f: Float => ensureType[T](math.floor(f.toDouble).toFloat)
    case d: Double => ensureType[T](math.floor(d))
    case bd: BigDecimal => ensureType[T](BigDecimal(math.floor(bd.toDouble)))
    case o => o
  }

  def ceilT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case f: Float => ensureType[T](math.ceil(f.toDouble).toFloat)
    case d: Double => ensureType[T](math.ceil(d))
    case bd: BigDecimal => ensureType[T](BigDecimal(math.ceil(bd.toDouble)))
    case o => o
  }

  def rintT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case f: Float => ensureType[T](math.rint(f.toDouble).toFloat)
    case d: Double => ensureType[T](math.rint(d))
    case bd: BigDecimal => ensureType[T](BigDecimal(math.rint(bd.toDouble)))
    case o => o
  }

  def roundT[T](t: T, scale: Int, mode: RoundingMode = RoundingMode.HALF_EVEN)(
    implicit e: HasEnsureType[T]): T = 
    t match {
      case f: Float =>
        ensureType[T](BigDecimal(f.toDouble).setScale(scale, mode).toFloat)
      case d: Double =>
        ensureType[T](BigDecimal(d).setScale(scale, mode).toDouble)
      case bd: BigDecimal => ensureType[T](bd.setScale(scale, mode))
      case o => o
    }

  def sqrtT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case f: Float => ensureType[T](math.sqrt(f))
    case d: Double => ensureType[T](math.sqrt(d))
    case b: Byte => ensureType[T](math.sqrt(b.toDouble))
    case s: Short => ensureType[T](math.sqrt(s.toDouble))
    case i: Int => ensureType[T](math.sqrt(i.toDouble))
    case l: Long => ensureType[T](math.sqrt(l.toDouble))
  }

  def sinT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case d: Double => ensureType[T](math.sin(d))
    case b: Byte => ensureType[T](math.sin(b.toDouble))
    case s: Short => ensureType[T](math.sin(s.toDouble))
    case i: Int => ensureType[T](math.sin(i.toDouble))
    case l: Long => ensureType[T](math.sin(l.toDouble))
    case f: Float => ensureType[T](math.sin(f.toDouble))
  }

  def cosT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case d: Double => ensureType[T](math.cos(d))
    case b: Byte => ensureType[T](math.cos(b.toDouble))
    case s: Short => ensureType[T](math.cos(s.toDouble))
    case i: Int => ensureType[T](math.cos(i.toDouble))
    case l: Long => ensureType[T](math.cos(l.toDouble))
    case f: Float => ensureType[T](math.cos(f.toDouble))
  }

  def tanT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case d: Double => ensureType[T](math.tan(d))
    case b: Byte => ensureType[T](math.tan(b.toDouble))
    case s: Short => ensureType[T](math.tan(s.toDouble))
    case i: Int => ensureType[T](math.tan(i.toDouble))
    case l: Long => ensureType[T](math.tan(l.toDouble))
    case f: Float => ensureType[T](math.tan(f.toDouble))
  }

  def asinT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case d: Double => ensureType[T](math.asin(d))
    case b: Byte => ensureType[T](math.asin(b.toDouble))
    case s: Short => ensureType[T](math.asin(s.toDouble))
    case i: Int => ensureType[T](math.asin(i.toDouble))
    case l: Long => ensureType[T](math.asin(l.toDouble))
    case f: Float => ensureType[T](math.asin(f.toDouble))
  }

  def acosT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case d: Double => ensureType[T](math.acos(d))
    case b: Byte => ensureType[T](math.acos(b.toDouble))
    case s: Short => ensureType[T](math.acos(s.toDouble))
    case i: Int => ensureType[T](math.acos(i.toDouble))
    case l: Long => ensureType[T](math.acos(l.toDouble))
    case f: Float => ensureType[T](math.acos(f.toDouble))
  }

  def cbrtT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case d: Double => ensureType[T](math.cbrt(d))
    case b: Byte => ensureType[T](math.cbrt(b.toDouble))
    case s: Short => ensureType[T](math.cbrt(s.toDouble))
    case i: Int => ensureType[T](math.cbrt(i.toDouble))
    case l: Long => ensureType[T](math.cbrt(l.toDouble))
    case f: Float => ensureType[T](math.cbrt(f.toDouble))
  }

  def atanT[T](t: T)(implicit e: HasEnsureType[T]): T = t match {
    case d: Double => ensureType[T](math.atan(d))
    case b: Byte => ensureType[T](math.atan(b.toDouble))
    case s: Short => ensureType[T](math.atan(s.toDouble))
    case i: Int => ensureType[T](math.atan(i.toDouble))
    case l: Long => ensureType[T](math.atan(l.toDouble))
    case f: Float => ensureType[T](math.atan(f.toDouble))
  }

  private[this] def asDouble(x: Any): Double = x match {
    case d: Double => d
    case b: Byte => b.toDouble
    case s: Short => s.toDouble
    case i: Int => i.toDouble
    case l: Long => l.toDouble
    case f: Float => f.toDouble
  }

  def atan2T[T](y: T, x: T)(implicit e: HasEnsureType[T]): T = y match {
    case d: Double => ensureType[T](math.atan2(d, asDouble(x)))
    case b: Byte => ensureType[T](math.atan2(b.toDouble, asDouble(x)))
    case s: Short => ensureType[T](math.atan2(s.toDouble, asDouble(x)))
    case i: Int => ensureType[T](math.atan2(i.toDouble, asDouble(x)))
    case l: Long => ensureType[T](math.atan2(l.toDouble, asDouble(x)))
    case f: Float => ensureType[T](math.atan2(f.toDouble, asDouble(x)))
  }
}

/**
  * This trait is the superclass of all the synthetic package trees in Terra.
  * The synthetic packages then extend this trait to inherit all their
  * symbols which are wired in from the AbstractDoubleTerraOps.
  */
trait TypeScope[Tuple <: TypeContext] {

  implicit val ops: TerraOps[Tuple]

  type QuantitySeries[A <: Quantity[A, T, Tuple], T] =
    IndexedSeq[QuantityRange[A, T, Tuple]]

  /* Quantity Types brought into scope with by importing packages extending 
   * this trait */
  type Dimensionless = DimensionlessLike[Tuple]
  lazy val Each = ops.dimensionlessOps.Each
  lazy val Percent = ops.dimensionlessOps.Percent
  lazy val Dozen = ops.dimensionlessOps.Dozen
  lazy val Score = ops.dimensionlessOps.Score
  lazy val Gross = ops.dimensionlessOps.Gross
  lazy val Dimensionless = ops.dimensionlessOps.Dimensionless

  /**
    * Provides implicit conversions that allow Numerics to lead in * and / by 
    * Time operations
    * {{{
    *    1.5 * Kilometers(10) should be(Kilometers(15))
    * }}}
    *
    * @param d Double
    */
  abstract class QuantityHelper[T, D](d: D, numeric: Numeric[D]) {

    type Time = TimeLike[Tuple]
    type Frequency = FrequencyLike[Tuple]

    def times[A <: Quantity[A, T, Tuple]](that: A): A = {
      implicit val e: HasEnsureType[T] = that.makeEnsureType
      that * ops.ensureType[T](d)
    }
    def times[A <: Quantity[A, T, Tuple]](
      that: SVector[A, T, Tuple]): SVector[A, T, Tuple] =
      if (that.coordinates.isEmpty) {
        SVector[A, T, Tuple]()
      } else {
        implicit val e: HasEnsureType[T] = that.coordinates(0).makeEnsureType
        that * ops.ensureType[T](d)
      }
    def div(that: Time): Frequency = {
      implicit val n = numeric
      Each(d) / that
    }
  }

  object DimensionlessConversions {

    import ops.dimensionlessOps.{ DimensionlessConversions => Convs }

    lazy val each = Convs.each
    lazy val percent = Convs.percent
    lazy val dozen = Convs.dozen
    lazy val score = Convs.score
    lazy val gross = Convs.gross
    lazy val hundred = Convs.hundred
    lazy val thousand = Convs.thousand
    lazy val million = Convs.million

    implicit class DimensionlessConversionsI[B](a: B)(implicit num: Numeric[B])
        extends Convs.DimensionlessConversions[B](a)

    implicit def dimensionlessToTI(d: Dimensionless): Double =
      Convs.dimensionlessToT(d)

    implicit object DimensionlessNumeric
        extends AbstractQuantityNumeric[DimensionlessLike[Tuple], Tuple](
      Dimensionless) {

      override def times(
        x: DimensionlessLike[Tuple], y: DimensionlessLike[Tuple]) = x * y
    }
  }
}
